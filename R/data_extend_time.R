###########♠
## function to duplicate data with datetime before the data time, for the same point
## data : need timestamp
## max_time_diff : maximum number of second to duplicate data lines before the effort time
## t_gap: duplicate with time intervals of t_gap
## accelerate : if TRUE, aggregate duplicated times by time intervals of t_gap, to decrease the resulting number of times

## return duplicated data wiht new columns :
## timestamp_AIS_to_extract with are the duplicated times (before the effort time)
## datetime_AIS_to_extract : same but in datetime format
## diffTime_effort_AIS : difference of time between effort time and duplicated time

#' Title
#'
#' @param data data with a column timestamp (numeric value of time)
#' @param max_time_diff number of seconds before the data time, when boat positions are considered/extracted.
#' @param t_gap interval of time where vessels positions are extracted, from the data time to "max_time_diff" seconds before.
#' @param accelerate TRUE or FALSE: if data times must be averaged within "average_at" seconds, to equlize times where vessels positions are extracted and decreased strongly computation time.
#' @param average_at number of seconds where times are averaged if accelerate = TRUE to decrease number of data time to extract. If average_at = 10, data times are averaged in the interval time-5:time+5.
#'
#' @return to add
#' @export
#'
#' @examples to add
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
    arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(datetime_AIS_to_extract = lubridate::as_datetime(timestamp_AIS_to_extract))

  return(data)

}
