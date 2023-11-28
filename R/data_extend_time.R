###########♠
## function to duplicate data with times before the data timestamp, to investigate the AIS presence before the time of the data.

#' data_extend_time
#'
#' @param data Data of interest for AIS extraction. Must contain a column "timestamp", "lon" and "lat" (numeric values).
#' @param max_time_diff number of seconds before the timestamp of every data timestamp, where boat positions are considered/extracted.
#' @param t_gap interval of time into which vessels positions are extracted, from the data timestamp up to "max_time_diff" seconds before. This defines also the time interval where boat are considered for the extraction.
#' @param accelerate TRUE or FALSE: if data timestamps must be averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#' @param average_at if accelerate = TRUE, average the data timestamps to +- average_at to decrease the number of data timestamp to process. This defines also the time interval where boat are considered for the extraction.
#'
#' @return the data frame with the lines duplicated for each "t_gap" to extract up to "max_time_diff". The output contains the columns:
#' timestamp_AIS_to_extract: timestamp for the extraction of the AIS for this line (= data timestamp if average_at = 0 & average_mmsi_at = 0 & accelerate = F).
#' diffTime_AIS_extraction_effort: difference, in seconds, between the timestamp to extract (timestamp_AIS_to_extract) and the data timestamp.
#' datetime_AIS_to_extract: datetime (ymd_hms) of timestamp_AIS_to_extract

#' @export
#'
#' @examples # to add
data_extend_time <- function(data,
                             max_time_diff = 30*60,
                             t_gap = 15,
                             accelerate = F,
                             average_at = 10) {

  # pack <- c("tidyverse", "dplyr")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  if (accelerate) {
    list_times <- seq(min(data$timestamp, na.rm = T) - (max_time_diff + t_gap), max(data$timestamp, na.rm = T) + t_gap, by = average_at)

    ref <- data %>%
      group_by(timestamp) %>%
      dplyr::mutate(timestamp_AIS_to_extract = list_times[which.min(abs(list_times - unique(timestamp)))],
                    diffTime_AIS_extraction_effort = timestamp_AIS_to_extract - unique(timestamp)) %>%
      ungroup()

    rm(list_times)

    if (t_gap < 0 | max_time_diff < 0) {
      stop("t_gap or max_time_diff lower than 0")
    } else if (max_time_diff > 0 & t_gap > 0) {
      data <- map_dfr(.x = 1:round(max_time_diff / t_gap, 0), .f = function(t) {
        return(ref %>%
                 dplyr::mutate(timestamp_AIS_to_extract = timestamp_AIS_to_extract - t * t_gap,
                               diffTime_AIS_extraction_effort = diffTime_AIS_extraction_effort - t * t_gap)
        )
      }) %>%
        rbind(ref)
    } else {
      data <- ref
    }

    rm(ref)

  } else {
    if (t_gap < 0 | max_time_diff < 0) {
      stop("t_gap or max_time_diff lower than 0")
    } else if (max_time_diff > 0 & t_gap > 0) {
#' Title
#'
#' @param t
#'
#' @return
#' @export
#'
#' @examples
      data <- map_dfr(.x = unique(c(seq(0, max_time_diff, t_gap), max_time_diff)), .f = function(t) {
        return(data %>%
                 dplyr::mutate(timestamp_AIS_to_extract = timestamp - t,
                               diffTime_AIS_extraction_effort = -t))
      })
    } else {
      data <- data %>%
        dplyr::mutate(timestamp_AIS_to_extract = timestamp,
                      diffTime_AIS_extraction_effort = 0)
    }
  }

  data <- data %>%
    distinct() %>%
    dplyr::arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(datetime_AIS_to_extract = lubridate::as_datetime(timestamp_AIS_to_extract))

  return(data)

}
