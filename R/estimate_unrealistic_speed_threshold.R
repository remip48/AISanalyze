#' Estimate a threshold for unrealistic speeds
#'
#' Estimates a threshold above which vessel speeds are considered
#' unrealistically high, based on the median and standard deviation
#' of observed vessel speeds.
#'
#' @param speed_kmh Numeric vector of vessel speeds in km/h.
#'
#' @return A numeric value representing the estimated speed threshold.
#'
#' @keywords internal
#' @noRd
estimate_unrealistic_speed_threshold <- function(speed_kmh) {
  return(
    15 +
      stats::median(speed_kmh[speed_kmh > 1], na.rm = TRUE) +
      5 * stats::sd(speed_kmh[speed_kmh > 1 & speed_kmh < stats::quantile(speed_kmh[speed_kmh > 1], .75)])
  )}
