#' function to extract the AIS position on a points
#'
#' @param data Data of interest for AIS extraction. Must contain a column "timestamp", "lon" and "lat" (numeric values).
#' @param data_mmsi AIS data. Must contain a column timestamp, lon, lat and mmsi (numeric value). the mmsi column is the identifier for vessel, and values can be replaced by the IMO for example, but the name of the column must be mmsi.
#' @param search_into_radius_m radius in which vessels positions are extracted.
#' @param duplicate_time  if vessels positions must be extracted only for data timestamp (TRUE), or every "t_gap" seconds, up to "max_time_diff" before the data timestamps.
#' @param max_time_diff number of seconds before the timestamp of every data timestamp, where boat positions are considered/extracted.
#' @param average_at if accelerate = TRUE, average the data timestamps to +- average_at to decrease the number of data timestamp to process. This defines also the time interval where boat are considered for the extraction.
#' @param t_gap interval of time into which vessels positions are extracted, from the data timestamp up to "max_time_diff" seconds before. This defines also the time interval where boat are considered for the extraction.
#' @param accelerate TRUE or FALSE: if data timestamps must be averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#'
#' @return return the input data with AIS extracted merged. Each line of input data is duplicated: a duplicate for each "t_gap" to extract up to "max_time_diff", and among these, duplicate for each MMSI present in the "radius" at the timestamp of interest. If no AIS are present in the radius, the columns dedicated to AIS data are filled with NA, so that no input data and no timestamp to extract is lost.
#' The output dataframe contains the columns of the input data, the columns of the AIS data (with "ais_" as prefix if the same column is already present in the input data), and the following columns:
#' distance_effort_ais_m: distance (meters) between the data point and the MMSI for this line (filled with NA if no MMSI).
#' timestamp_AIS_to_extract: timestamp for the extraction of the AIS for this line (= data timestamp if average_at = 0 & average_mmsi_at = 0 & accelerate = F).
#' diffTime_AIS_extraction_effort: difference, in seconds, between the timestamp to extract (timestamp_AIS_to_extract) and the data timestamp.
#' datetime_AIS_to_extract: datetime (ymd_hms) of timestamp_AIS_to_extract
#' hour_AIS_to_extract: hour of timestamp_AIS_to_extract
#' time_travelled: number of seconds since the last reception of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessel since the last reception of an AIS signal.
#' id_ais_data_initial: identifier of the row line in the ais data, ordered, corrected and cleaned. Use for internal computation. For interpolated data, id_ais_data_initial is the same than the next real existing line.
#' station: if the MMSI is a station or not.
#' high_speed: if the MMSI is a high speed craft (used for aircraft) or not.
#' any_NA_speed_kmh: if any of the MMSI point has a value of speed of NA (so distance_travelled or time_travelled has a issue and the MMSI points must be checked). Should not occur.
#' n_point_mmsi_initial_data: number of point of the MMSI in the initial AIS data, removing firstly the inexisting longitude and latitude points.
#' id_mmsi_point_initial: identifier for the MMSI point in the ordered, corrected and cleaned AIS data.
#' speed_kmh_corrected: if the speed of this line has been corrected or not.
#' interpolated: if this AIS position is an interpolation or not.
#' diffTime_AIS_effort: difference, in seconds, between the AIS position and the data timestamp.
#' @export
#'
#' @examples # to add
AISextract <- function(data,
                             data_mmsi,
                             search_into_radius_m = 50000,
                             duplicate_time = F,
                             max_time_diff = 0,
                             average_at = 0,
                             t_gap = 10*60,
                             accelerate = F
) {

  data_mmsi <- data_mmsi[data_mmsi$timestamp > (min(data$timestamp, na.rm = T) - (max_time_diff + t_gap + average_at + average_mmsi_at/2)) &
                           data_mmsi$timestamp < (max(data$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2), ]

  if (duplicate_time) {
    data <- DATAextend_time(data = data, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
    # } else if (accelerate) {
    #   source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-1_DATAextend_time_data.R")
    #   data <- DATAextend_time(data = data, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap)
  } else if (accelerate) {
    data <- DATAextend_time(data = data, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
  } else {
    data <- data %>%
      dplyr::mutate(timestamp_AIS_to_extract = timestamp,
             diffTime_AIS_extraction_effort = 0)
  }

  data <- data %>%
    # dplyr::arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(timestamp_eff = timestamp) %>%
    dplyr::select(!"timestamp") %>%
    dplyr::mutate(idd_effort = 1:n())

  # print("Extracting AIS on data")

  return_columns <- function(.x) {
    return(.x)
  }

  # which_point <- function(.x, point) {
  #   return(.x[point])
  # }

  # NA_return <- function(.x) {
  #   return(NA)
  # }

  # pb <- txtProgressBar(min = 0, max = length(unique(data$timestamp_AIS_to_extract)), style = 3)

  time_ais <- map_dfr(unique(data$timestamp_AIS_to_extract), function(dt) {

    # print(match(dt, unique(data$timestamp_AIS_to_extract)))
    # setTxtProgressBar(pb, match(dt, list_t_extract))

    eff_dt <- data[data$timestamp_AIS_to_extract == dt,]

    mmsi_ref <- data_mmsi[data_mmsi$timestamp > (dt - t_gap - average_at - average_mmsi_at/2) &
                            data_mmsi$timestamp < (dt + t_gap + average_at + average_mmsi_at/2) &
                            data_mmsi$ais_X >= (min(eff_dt$X) - search_into_radius_m) & data_mmsi$ais_X <= (max(eff_dt$X) + search_into_radius_m) &
                            data_mmsi$ais_Y >= (min(eff_dt$Y) - search_into_radius_m) & data_mmsi$ais_Y <= (max(eff_dt$Y) + search_into_radius_m),]

    if (nrow(mmsi_ref) > 1) {
      mmsi_ref <- mmsi_ref %>%
        dplyr::mutate(idd_ais = 1:n())

      mmsi_eff <- mmsi_ref %>%
        group_by(mmsi) %>%
        dplyr::reframe(point = which.min(abs(timestamp - dt)),
                       # across(colnames(data_mmsi)[colnames(data_mmsi) != "mmsi"], ~ which_point(.x, point))
                       idd_ais = idd_ais[point],
                       ais_X = ais_X[point],
                       ais_Y = ais_Y[point],
                       ais_timestamp = timestamp[point]
        )

      out <- eff_dt %>%
        group_by(idd_effort) %>%
        dplyr::reframe(#across(colnames(eff_dt)[colnames(eff_dt) != "idd_effort"], ~ return_columns(.x)),
          mmsi_eff %>%
            dplyr::mutate(distance_effort_ais_m = sqrt((ais_X-X)^2 + (ais_Y-Y)^2))
        )

      out <- out[out$distance_effort_ais_m <= search_into_radius_m, ] %>%
        left_join(eff_dt, by = "idd_effort") %>%
        left_join(mmsi_ref %>%
                    dplyr::select(-c(ais_X, ais_Y, mmsi, timestamp)), by = "idd_ais") %>%
        dplyr::select(-idd_ais)

      rm(mmsi_eff)
    } else {
      out <- eff_dt
    }

    rm(eff_dt)
    rm(mmsi_ref)

    return(out)

  })

  if (!("ais_timestamp" %in% colnames(time_ais))) {
    cat("No AIS data for data processed between", min(time_ais$datetime_AIS_to_extract), "and", max(time_ais$datetime_AIS_to_extract), "\n")
    time_ais <- time_ais %>%
      dplyr::mutate(ais_timestamp = NA)
  }

  time_ais <- time_ais %>%
    dplyr::mutate(diffTime_AIS_effort = ais_timestamp - timestamp_eff,
                  diffTime_AIS_extraction_effort = timestamp_AIS_to_extract - timestamp_eff) %>%
    dplyr::select(-c("point")[c("point") %in% colnames(time_ais)])

  if (any(!(data$idd_effort %in% time_ais$idd_effort))) {
    time_ais <- map_dfr(list(time_ais,
                             data[!(data$idd_effort %in% time_ais$idd_effort), ]),
                        function(l) {return(l)})
  }

  gc()

  # if (limited_to_AIS_time) {
  #   time_ais <- time_ais %>%
  #     filter(timestamp_AIS_to_extract >= (min(AIS$timestamp, na.rm = T) - t_gap) & timestamp_AIS_to_extract <= (max(AIS$timestamp, na.rm = T) + t_gap))
  # }

  time_ais <- time_ais %>%
    dplyr::mutate(timestamp = timestamp_eff) %>%
    dplyr::select(!c("timestamp_eff", "idd_effort"))

  # if (nrow(time_ais) == 0) {
  #   time_ais <- data %>%
  #     dplyr::mutate(timestamp = timestamp_eff) %>%
  #     dplyr::select(!"timestamp_eff")
  # }

  rm(data)

  return(time_ais)

}
