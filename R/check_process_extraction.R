#' Check that at least some AIS data were extracted
#'
#' @param time_ais
#'
#' @return
#'
#' @examples
check_process_extraction <- function(time_ais) {
  if (!("ais_timestamp" %in% colnames(time_ais))) {
    cat("\nNo AIS data for data processed between", min(time_ais$datetime_AIS_to_extract), "and", max(time_ais$datetime_AIS_to_extract), "\n")
    time_ais <- time_ais %>%
      dplyr::mutate(ais_timestamp = NA)
  }

  return(time_ais)
}
