#############################
## function to extract the AIS position on a points
## data : data where AIS must be extracted, must contain timestamp, lon and lat columns
## AIS : ais data, must contain lon, lat, timestamp
## max_time_diff : number of seconds before the time t of a point, where AIS must be extract on this point
## t_gap : time interval to extract AIS data from time of effort t to t - max_time_diff, and also time interval in which AIS data are searching around the effort time
## onLand : if AIS data contain information for onLand
## limited_to_AIS_time : if output data must be limited to the range of time contained into the AIS
## filter_radius_m: NA to not filter, or distance in m to return only lines with these distances

## this function must be specifically started after the previous functions

## edit 20230412: with the filter added | is.na(distance_effort_ais_m), should return all the initial lines of data

# source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-1_extend_data_time.R")

#' Extract vessels data at desired times in a desired radius. Used for AISextract. Use only if sure about this.
#'
#' @param data Your data frame, with a column timestamp, lon and lat (numeric value of time, longitude, latitude)
#' @param data_mmsi AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity)
#' @param search_into_radius_m radius  in which vessels positions are returned
#' @param duplicate_time  if vessels positions must be extracted only for data times (TRUE) or every "t_gap" seconds up to max_time_diff before data times.
#' @param max_time_diff number of seconds before the data time, when boat positions are considered/extracted.
#' @param average_at number of seconds where times are averaged if accelerate = TRUE to decrease number of data time to extract. If average_at = 10, data times are averaged in the interval time-5:time+5.
#' @param t_gap interval of time where vessels positions are extracted, from the data time to "max_time_diff" seconds before.
#' @param accelerate TRUE or FALSE: if data times must be averaged within "average_at" seconds, to equlize times where vessels positions are extracted and decreased strongly computation time.
#'
#' @return to add
#' @export
#'
#' @examples # to add
data_extract_ais <- function(data,
                                 data_mmsi,
                                 search_into_radius_m = 20000,
                                 duplicate_time = T,
                                 max_time_diff = 0,
                                 average_at = 0,
                                 t_gap,
                                 # list_t_extract = NA,
                                 # onLand = F,
                                 accelerate = T
                                 # limited_to_AIS_time = F
                                 ) {

  # pack <- c("tidyverse", "dplyr")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  data_mmsi <- data_mmsi %>%
    dplyr::filter(timestamp > (min(data$timestamp, na.rm = T) - (max_time_diff + t_gap + average_at)) &
             timestamp < (max(data$timestamp, na.rm = T) + t_gap + average_at))

  if (duplicate_time) {
    data <- data_extend_time(data = data, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
    # } else if (accelerate) {
    #   source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-1_data_extend_time_data.R")
    #   data <- data_extend_time(data = data, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap)
  } else if (accelerate) {
    data <- data_extend_time(data = data, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
  } else {
    data <- data %>%
      dplyr::mutate(timestamp_AIS_to_extract = timestamp,
             diffTime_AIS_extraction_effort = 0)
  }

  data <- data %>%
    dplyr::arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(timestamp_eff = timestamp) %>%
    dplyr::select(!"timestamp") %>%
    dplyr::mutate(idd_effort = 1:n())

  # print("Extracting AIS on data")

  return_columns <- function(.x) {
    return(.x)
  }

  which_point <- function(.x, point) {
    return(.x[point])
  }

  # NA_return <- function(.x) {
  #   return(NA)
  # }

  # pb <- txtProgressBar(min = 0, max = length(unique(data$timestamp_AIS_to_extract)), style = 3)

  time_ais <- map_dfr(unique(data$timestamp_AIS_to_extract), function(dt) {

    # print(match(dt, unique(data$timestamp_AIS_to_extract)))
    # setTxtProgressBar(pb, match(dt, list_t_extract))

    eff_dt <- data[data$timestamp_AIS_to_extract == dt,]

    mmsi_eff <- data_mmsi[data_mmsi$timestamp > (dt - t_gap - average_at) &
                            data_mmsi$timestamp < (dt + t_gap + average_at),] %>%
      group_by(mmsi) %>%
      dplyr::reframe(point = which.min(abs(timestamp - dt)),
                     across(colnames(data_mmsi)[colnames(data_mmsi) != "mmsi"], ~ which_point(.x, point)))

    out <- eff_dt %>%
      group_by(idd_effort) %>%
      dplyr::reframe(across(colnames(eff_dt)[colnames(eff_dt) != "idd_effort"], ~ return_columns(.x)),
                     mmsi_eff %>%
                       dplyr::mutate(distance_effort_ais_m = sqrt((ais_X-X)^2 + (ais_Y-Y)^2)) %>%
                       filter(distance_effort_ais_m <= search_into_radius_m)
      )

    rm(eff_dt)
    rm(mmsi_eff)

    return(out)

  }) %>%
    dplyr::mutate(diffTime_AIS_effort = timestamp - timestamp_ofEffort) %>%
    dplyr::select(-c(X, Y, point, ais_X, ais_Y))

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
    dplyr::select(!"timestamp_eff")

  # if (nrow(time_ais) == 0) {
  #   time_ais <- data %>%
  #     dplyr::mutate(timestamp = timestamp_eff) %>%
  #     dplyr::select(!"timestamp_eff")
  # }

  rm(data)

  return(time_ais)

}
