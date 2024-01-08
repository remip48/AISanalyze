#' Extract the AIS position for desired location and times
#'
#' Extract the AIS position around the locations and at the time of the data
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param search_into_radius_m radius (kilometer) where the MMSIs are extracted and returned.
#' @param duplicate_time if TRUE, extend (and duplicate) the data to past timestamps, to investigate the past presence of vessels at the data locations: extend the data timestamps up to "max_time_diff" number of seconds before the timestamps, by steps of "t_gap" number of seconds.
#' @param max_time_diff if duplicate_time = TRUE, extend (and duplicate) the data to past timestamps, to investigate the past presence of vessels at the data locations: extend the data timestamps up to "max_time_diff" number of seconds before the timestamps, by steps of "t_gap" number of seconds.
#' @param t_gap see "max_time_diff". Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction (otherwise other AIS data are filtered out).
#' @param accelerate if TRUE, data timestamps are averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#' @param average_at if accelerate = TRUE, the data timestamps are approximated to within to "average_at" number of seconds. This, to decrease the number of data timestamps to process. Necessary for large data timestamps to extract. Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction, in addition to "t_gap" parameter (otherwise other AIS data are filtered out).
#'
#' @return return the input data with the AIS extracted merged in the dataframe: each line of input data is duplicated by timestamp to extract (every "t_gap" number of seconds up to "max_time_diff" number of seconds). All these lines are duplicated for each MMSI present in the area at the moment of the extraction. If no AIS are present in the radius at this moment, the columns dedicated to AIS data are filled with NA, so that no input data and no timestamp to extract is lost.
#' The output dataframe contains the columns of the input data, the columns of the AIS data (with "ais_" as prefix if the same column is already present in the input data), and the following columns:
#' \itemize{
#' \item distance_effort_ais_m: distance (meters) between the data location and the MMSI at this time (filled with NA if no MMSI).
#' \item timestamp_AIS_to_extract: timestamp for the extraction of the AIS (approximated with "average_at" number of seconds if accelerate = TRUE).
#' \item diffTime_AIS_extraction_effort: difference (in seconds) between the timestamp to extract (timestamp_AIS_to_extract) and the real data timestamp.
#' \item datetime_AIS_to_extract: datetime of timestamp_AIS_to_extract.
#' \item diffTime_AIS_effort: difference, in seconds, between the AIS data and the data timestamp: can be different from the difference between the timestamp of the extraction and the real data timestamp (diffTime_AIS_extraction_effort) due to the parameter "average_at" & "t_gap"}
#'

AISextract <- function(data,
                       ais_data,
                       search_into_radius_m = 50000,
                       duplicate_time = F,
                       max_time_diff = 0,
                       t_gap = 10*60,
                       accelerate = F,
                       average_at = 0
) {

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  ais_data <- ais_data[ais_data$timestamp > (min(data$timestamp, na.rm = T) - (max_time_diff + t_gap + average_at)) &
                           ais_data$timestamp < (max(data$timestamp, na.rm = T) + t_gap + average_at), ]

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

    mmsi_ref <- ais_data[ais_data$timestamp > (dt - t_gap - average_at) &
                            ais_data$timestamp < (dt + t_gap + average_at) &
                            ais_data$ais_X >= (min(eff_dt$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_dt$X) + search_into_radius_m) &
                            ais_data$ais_Y >= (min(eff_dt$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_dt$Y) + search_into_radius_m),]

    if (nrow(mmsi_ref) > 1) {
      mmsi_ref <- mmsi_ref %>%
        dplyr::mutate(idd_ais = 1:n())

      mmsi_eff <- mmsi_ref %>%
        group_by(mmsi) %>%
        dplyr::reframe(point = which.min(abs(timestamp - dt)),
                       # across(colnames(ais_data)[colnames(ais_data) != "mmsi"], ~ which_point(.x, point))
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
    cat("\nNo AIS data for data processed between", min(time_ais$datetime_AIS_to_extract), "and", max(time_ais$datetime_AIS_to_extract), "\n")
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
