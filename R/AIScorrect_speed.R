#' Correct GPS errors in AIS tracks
#'
#' Detects and corrects GPS errors and delayed AIS messages that generate
#' unrealistic vessel speeds, travelled distances, and travel times.
#' Note: For consecutive GPS errors, only the first point is removed to
#' avoid overcorrection.
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric. Another vessel identifier may be used if
#'   the column is named `mmsi`.
#' @param crs_meters CRS (metres) used to calculate travelled distances.
#'   Defaults to EPSG:3035.
#' @param threshold_speed_to_correct Speed threshold (km/h) above which
#'   observations are corrected.
#' @param threshold_speed_to_correct_function a function to estimate vessel-specific
#'   speed thresholds. The function can use all columns of ais_data as argument
#'   (such as `speed_kmh`, `distance_travelled` or `time_travelled`). If set to
#'   `NULL`, an internal function is used (see `details`).
#' @param nb_cores Number of CPU cores used.
#' @param outfile File used to save logs.
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
#' @details
#' When `threshold_speed_to_correct_function` is set to `NULL`, a vessel-specific
#' speed threshold is estimated from the observed vessel `speed_kmh`. The
#' threshold is calculated as:
#'
#' \deqn{
#'  T = 15 + \operatorname{median}(v_{>1}) +
#'    5 \times \operatorname{SD}(v_{1 < v < Q_{0.75}})
#' }{
#'  T = 15 + median(v > 1) +
#'    5 * SD(1 < v < 75th percentile)
#' }
#'
#' where \eqn{v_{>1}} represents vessel speeds greater than 1 km/h, and
#' \eqn{v_{1 < v < Q_{0.75}}} represents vessel speeds between 1 km/h and the
#' 75th percentile. The equation is designed to capture the typical
#' travelling speed of the vessel while accounting for variation in its
#' observed travelling speeds.
#'
#' @examples
#' data("ais")
#'
#' # use only a sample for the example:
#' ais <- ais[ais$mmsi %in% ais$mmsi[1:5], ]
#'
#' # Define the Unix time (seconds since 1970-01-01)
#' ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))
#'
#' # calculate the travelled distance, time, and speed:
#' ais <- AIStravel(ais_data = ais)
#'
#' # Correct speed:
#' out <- AIScorrect_speed(ais_data = ais,
#'                         crs_meters = 3035)
#' @export

AIScorrect_speed <- function(ais_data,
                             crs_meters = 3035,
                             threshold_speed_to_correct = 100,
                             threshold_speed_to_correct_function = NULL,
                             nb_cores = 1,
                             outfile = tempfile()
) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))
  assertthat::assert_that(is.numeric(threshold_speed_to_correct))
  assertthat::assert_that("time_travelled" %in% colnames(ais_data) & "distance_travelled" %in% colnames(ais_data) & "speed_kmh" %in% colnames(ais_data),
                          msg = "Please first run AIStravel() to calculate speed, distance and time travelled.")

  init_cols <- colnames(ais_data)

  if (all(is.null(threshold_speed_to_correct_function))) {
    threshold_speed_to_correct_function <- estimate_unrealistic_speed_threshold
  }

  assign_mmsi_to_core <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(-n) %>%
    dplyr::mutate(core = rep(1:nb_cores, ceiling(dplyr::n() / nb_cores))[1:dplyr::n()]) %>%
    dplyr::group_by(core) %>%
    dplyr::mutate(split_datasets = floor(cumsum(n) / 50000)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(core = paste(core, split_datasets))

  ais_data <- purrr::map(unique(assign_mmsi_to_core$core), function(co) {
    ais_data %>%
      dplyr::filter(mmsi %in% (assign_mmsi_to_core %>%
                                 dplyr::filter(core == co) %>%
                                 dplyr::pull(mmsi)))
  })

  cl <- parallel::makeCluster(nb_cores, outfile = outfile)
  doParallel::registerDoParallel(cl)

  corrected_data <- foreach::foreach(ais_data_core = ais_data,
                          .export = c("add_coordinates_meters"),
                          .noexport = c("assign_mmsi_to_core", "ais_data"),
                          .packages = c("dplyr", "sf")
  ) %dopar% {
    ais_data <- ais_data_core %>%
      add_coordinates_meters(., crs_meters = crs_meters) %>%
      sf::st_drop_geometry() %>%
      dplyr::arrange(mmsi, timestamp) %>%
      dplyr::mutate(id_ais_data_initial = 1:dplyr::n()) %>%
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(last_row = 1:dplyr::n(),
                    last_row = ifelse(last_row == dplyr::n(), TRUE, FALSE),
                    id_mmsi_point_initial = 1:dplyr::n()) %>%
      dplyr::ungroup()

    strange_speed <- ais_data %>%
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_function(speed_kmh)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(id_mmsi_point_initial != 1 & !last_row) %>%
      dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                     threshold_speed_to_correct,
                                                     threshold_strange_speed)) %>%
      dplyr::filter(speed_kmh > threshold_speed_to_correct | speed_kmh > threshold_strange_speed)

    ## extract only rows with strange speed that are not consecutive in the dataset
    strange_speed <- strange_speed[c(TRUE, (strange_speed$id_ais_data_initial[-1] - strange_speed$id_ais_data_initial[-nrow(strange_speed)]) >= 2), ]

    strange_speed <- strange_speed$id_ais_data_initial

    ## points for which the error is actually on the previous point
    short_time <- sort(ais_data$id_ais_data_initial[ais_data$id_mmsi_point_initial != 1 & !ais_data$last_row &
                                                      ((ais_data$distance_travelled <= 1 & ais_data$time_travelled > 60 * 5) | ais_data$distance_travelled == 0 | ais_data$speed_kmh < 0.001 | ais_data$time_travelled == 0)])

    strange_speed <- sort(unique(c(strange_speed[!(strange_speed %in% short_time)], short_time)))

    ## avoid consecutive GPS detected (but not always actual) errors to be both removed: only the first error is removed
    strange_speed <- strange_speed[c(TRUE, (strange_speed[-1] - strange_speed[-length(strange_speed)]) >= 2)]

    return(ais_data %>%
             dplyr::filter(!(id_ais_data_initial %in% strange_speed)) %>%
             dplyr::mutate(speed_to_correct = ifelse(id_ais_data_initial %in% (strange_speed + 1), TRUE, FALSE)))
  }

  parallel::stopCluster(cl)

  corrected_data <- purrr::map_dfr(corrected_data, rbind)

  lines_to_correct <- which(corrected_data$speed_to_correct)

  if (length(lines_to_correct) > 0) {
    correct_speed <- corrected_data[unique(c(lines_to_correct - 1, lines_to_correct)), ] %>%
      dplyr::select(-(c(time_travelled, distance_travelled, speed_kmh))) %>%
      dplyr::left_join(AIStravel(.,
                                 crs_meters = crs_meters) %>%
                         dplyr::select(time_travelled, distance_travelled, speed_kmh, mmsi, timestamp),
                       by = c("mmsi", "timestamp")) %>%
      dplyr::filter(speed_to_correct) %>%
      dplyr::mutate(speed_kmh_corrected = TRUE)

    corrected_data <- corrected_data[-lines_to_correct, ] %>%
      dplyr::mutate(speed_kmh_corrected = FALSE) %>%
      rbind(correct_speed) %>%
      dplyr::arrange(id_ais_data_initial)
  } else {
    corrected_data$speed_kmh_corrected <- FALSE
  }

  return(corrected_data %>%
           dplyr::select(dplyr::all_of(c(init_cols, "speed_kmh_corrected"))))
}
