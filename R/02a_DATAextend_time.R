#' Extend timestamps of data to past timestamps
#'
#' Extend timestamps of data to past timestamps, by duplicating the data lines (except timestamp, which goes from the timestamp of the data to "max_time_diff" seconds before, by steps of "t_gap"). Used in internal functions to investigate the AIS presence before the timestamp of the data.
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param max_time_diff if duplicate_time = TRUE, extend (and duplicate) the data to past timestamps, to investigate the past presence of vessels at the data locations: extend the data timestamps up to "max_time_diff" number of seconds before the timestamps, by steps of "t_gap" number of seconds.
#' @param t_gap see "max_time_diff". Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction, in addition to "average_at" parameter (otherwise other AIS data are filtered out).
#' @param accelerate if TRUE, data timestamps are averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#' @param average_at if accelerate = TRUE, the data timestamps are approximated to within to "average_at" number of seconds. This, to decrease the number of data timestamps to process. Necessary for large data timestamps to extract. Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction, in addition to "t_gap" parameter (otherwise other AIS data are filtered out).
#'
#' @return The data frame with the lines duplicated for each timestamps, from the data timestamps up to "max_time_diff" by steps of "t_gap". Contains the columns:
#' \itemize{
#' \item timestamp_AIS_to_extract: timestamp for the extraction of the AIS (approximated with "average_at" number of seconds if accelerate = TRUE).
#' \item diffTime_AIS_extraction_effort: difference (in seconds) between the timestamp to extract (timestamp_AIS_to_extract) and the real data timestamp.
#' \item datetime_AIS_to_extract: datetime of timestamp_AIS_to_extract.
#' }
#'
DATAextend_time <- function(data,
                            max_time_diff = 1 * 60 * 60,
                            t_gap = 30,
                            accelerate = T,
                            average_at = 30) {

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
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
      dplyr::group_by(timestamp) %>%
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
      data <- purrr::map_dfr(.x = unique(c(seq(0, max_time_diff, t_gap), max_time_diff)), .f = function(t) {
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
    dplyr::distinct() %>%
    dplyr::arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(datetime_AIS_to_extract = lubridate::as_datetime(timestamp_AIS_to_extract))

  return(data)

}
