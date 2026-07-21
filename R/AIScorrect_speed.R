#' Correct GPS errors in AIS tracks
#'
#' Detects and corrects GPS errors and delayed AIS messages that generate
#' unrealistic vessel speeds, travelled distances, and travel times.
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric. Another vessel identifier may be used if
#'   the column is named `mmsi`.
#' @param crs_meters CRS (metres) used to calculate travelled distances.
#'   Defaults to EPSG:3035.
#' @param threshold_speed_to_correct Speed threshold (km/h) above which
#'   observations are corrected.
#' @param threshold_speed_to_correct_expr Function returning a vessel-specific
#'   speed threshold from `speed_kmh`.
#'
#' @return The input data with corrected travel metrics and the following
#'   columns:
#'   \itemize{
#'   \item `speed_kmh_corrected`: Whether the speed was corrected.
#'   \item `time_travelled`: Travel time (s).
#'   \item `distance_travelled`: Travelled distance (m).
#'   \item `speed_kmh`: Vessel speed (km/h).
#'   }
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#'
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .)
#'
#' out <- AIScorrect_speed(ais_data = ais,
#'                  crs_meters = 3035,
#'                  threshold_speed_to_correct = 100)}
#' @export

AIScorrect_speed <- function(ais_data,
                             crs_meters = 3035,
                             threshold_speed_to_correct = 100,
                             threshold_speed_to_correct_expr = function(speed_kmh) {
                               return(
                                 15 + stats::median(speed_kmh[speed_kmh > 1], na.rm = T) + 5 * stats::sd(speed_kmh[speed_kmh > 1 & speed_kmh < stats::quantile(speed_kmh[speed_kmh > 1], .75)])
                                 )}
) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))
  assertthat::assert_that(is.numeric(threshold_speed_to_correct))

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    stop("Please run AIStravel() before AIScorrect_speed()")
  }

  init_cols <- colnames(ais_data)

  cat(
    "For consecutive GPS errors, only the first point is removed to avoid overcorrection.\nHigh-speed craft are not corrected.\n"
  )

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(mmsi, timestamp) %>%
    dplyr::mutate(id_ais_data_initial = 1:dplyr::n()) %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(last_row = 1:dplyr::n(),
                  last_row = ifelse(last_row == dplyr::n(), T, F),
                  id_mmsi_point_initial = 1:dplyr::n()) %>%
    dplyr::ungroup()

  strange_speed <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr(speed_kmh)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(id_mmsi_point_initial != 1 & !last_row) %>%
    dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                   threshold_speed_to_correct,
                                                   threshold_strange_speed)) %>%
    dplyr::filter((speed_kmh > threshold_speed_to_correct | speed_kmh > threshold_strange_speed) &
                    (time_travelled < 5*60*60))

  ## extract only rows with strange speed that are not consecutive in the dataset
  strange_speed <- strange_speed[c(T, (strange_speed$id_ais_data_initial[-1] - strange_speed$id_ais_data_initial[-nrow(strange_speed)]) >= 2), ]

  strange_speed <- strange_speed$id_ais_data_initial

  ## points for which the error is actually on the previous point
  short_time <- sort(ais_data$id_ais_data_initial[ais_data$id_mmsi_point_initial != 1 & !ais_data$last_row &
                                                    ((ais_data$distance_travelled <= 1 & ais_data$time_travelled > 60 * 5) | ais_data$distance_travelled == 0 | ais_data$speed_kmh < 0.001 | ais_data$time_travelled == 0)])

  strange_speed <- sort(unique(c(strange_speed[!(strange_speed %in% short_time)], short_time)))

  ## avoid consecutive GPS detected (but not always actual) errors to be both removed: only the first error is removed
  strange_speed <- strange_speed[c(T, (strange_speed[-1] - strange_speed[-length(strange_speed)]) >= 2)]
  it_sp <- 0

  ## recalculate the time/speed/distance travelled for points consecutive to removed errors
  if (length(strange_speed) > 0) {
    to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ] %>%
      dplyr::select(-(c(time_travelled, distance_travelled, speed_kmh))) %>%
      dplyr::left_join(AIStravel(.,
                          crs_meters = crs_meters) %>%
                  dplyr::select(time_travelled, distance_travelled, speed_kmh, mmsi, timestamp),
                by = c("mmsi", "timestamp"))

    ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
      dplyr::mutate(speed_kmh_corrected = F) %>%
      rbind(to_correct %>%
              dplyr::filter(id_ais_data_initial %in% (strange_speed + 1)) %>%
              dplyr::mutate(speed_kmh_corrected = T)) %>%
      dplyr::arrange(id_ais_data_initial)
  }

  return(ais_data %>%
           dplyr::select(dplyr::all_of(c(init_cols, "speed_kmh_corrected"))))
}
