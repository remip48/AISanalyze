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
#' @param mmsi_time_to_order Logical. If `TRUE`, orders the data by `mmsi` and
#'   `timestamp` before processing.
#' @param correct_high_speed_craft Logical. Correct high-speed craft (typically
#'   aircraft).
#' @param threshold_speed_to_correct Speed threshold (km/h) above which
#'   observations are corrected.
#' @param threshold_speed_to_correct_expr Function returning a vessel-specific
#'   speed threshold from `speed_kmh`.
#' @param time_stop Maximum time gap (seconds) used for interpolation.
#' @param quantile_station Distance quantile (0-1) used to identify stationary
#'   MMSIs.
#' @param threshold_distance_station Distance threshold (metres) for stationary
#'   MMSIs.
#' @param quantile_high_speed Speed quantile (0-1) used to identify high-speed
#'   craft.
#' @param threshold_high_speed Speed threshold (km/h) for high-speed craft.
#'
#' @return The input data with corrected travel metrics and the following
#'   columns:
#'   \itemize{
#'   \item `speed_kmh_corrected`: Whether the speed was corrected.
#'   \item `time_travelled`: Travel time (s).
#'   \item `distance_travelled`: Travelled distance (m).
#'   \item `speed_kmh`: Vessel speed (km/h).
#'   \item `station`: Whether the MMSI is stationary.
#'   \item `high_speed`: Whether the MMSI is high-speed.
#'   }
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' library(dplyr)
#' library(lubridate)
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             time_stop = 5*60*60,
#'             mmsi_time_to_order = T,
#'             return_sf = F,
#'             return_3035_coords = F)
#'
#' AIScorrect_speed(ais_data = ais,
#'                  mmsi_time_to_order = T,
#'                  threshold_speed_to_correct = 100,
#'                  time_stop = 5*60*60)}
#' @export

AIScorrect_speed <- function(ais_data,
                             crs_meters = 3035,
                             mmsi_time_to_order = TRUE,
                             threshold_speed_to_correct = 100,
                             threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 1], na.rm = T) +
                                                                                              sd(speed_kmh[speed_kmh > 1 & speed_kmh < quantile(speed_kmh[speed_kmh > 1], .75)]) * 5 + 15))},
                             time_stop = 5*60*60,
                             nb_iteration = 1,
                             nb_iteration_successive_strange = 1,
                             correct_high_speed_craft = FALSE,
                             quantile_station = 0.975,
                             threshold_distance_station = 1,
                             quantile_high_speed = 0.97,
                             threshold_high_speed = 110
) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))
  assertthat::assert_that(is.numeric(threshold_speed_to_correct))
  assertthat::assert_that(is.numeric(time_stop))

  init_cols <- colnames(ais_data)

  cat("In case of consecutive GPS points detected as an error for single vessels, only the first GPS point will be removed (and others will be kept) to avoid overcorrecting tracks with usual patterns, and as most GPS errors occur over one point only.\n")

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)

    # if (!("id_ais_data_initial" %in% colnames(ais_data))) {
    #   cat("Rewriting id_ais_data_initial\n")
    # }

    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    ais_data <- AIStravel(ais_data = ais_data,
                          crs_meters = crs_meters,
                          time_stop = time_stop,
                          mmsi_time_to_order = F,
                          return_sf = F,
                          return_3035_coords = T)
  }

  if (!("id_ais_data_initial" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if (!("id_mmsi_point_initial" %in% colnames(ais_data)) | !("station" %in% colnames(ais_data)) | !("high_speed" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      left_join(AISidentify_stations_aircraft(.,
                                              crs_meters = crs_meters,
                                              quantile_station = quantile_station,
                                              threshold_distance_station = threshold_distance_station,
                                              quantile_high_speed = quantile_high_speed,
                                              threshold_high_speed = threshold_high_speed) %>%
                  dplyr::select(mmsi, timestamp, station, high_speed, id_mmsi_point_initial), by = c("mmsi", "timestamp"))
  }

  if (correct_high_speed_craft) {
    ais_data <- ais_data %>%
      dplyr::mutate(real_high_speed = high_speed,
                    high_speed = F)
  }

  ais_data <- ais_data %>%
    group_by(mmsi) %>%
    dplyr::mutate(last_row = 1:n(),
                  last_row = ifelse(last_row == n(), T, F)) %>%
    ungroup()

  strange_speed <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr(speed_kmh)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(id_mmsi_point_initial != 1 & !last_row) %>%
    dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                   threshold_speed_to_correct,
                                                   threshold_strange_speed)) %>%
    dplyr::filter((speed_kmh > threshold_speed_to_correct |
                     speed_kmh > threshold_strange_speed) &
                    (time_travelled < time_stop &
                       !(high_speed) &
                       !(station)))

  ## extract only rows with strange speed that are not consecutive in the dataset
  strange_speed <- strange_speed[c(T, (strange_speed$id_ais_data_initial[-1] - strange_speed$id_ais_data_initial[-nrow(strange_speed)]) >= 2), ]

  strange_speed <- strange_speed$id_ais_data_initial

  ## points for which the error is actually on the previous point
  short_time <- sort(ais_data$id_ais_data_initial[!(ais_data$high_speed) &
                                                    !(ais_data$station) &
                                                    ais_data$id_mmsi_point_initial != 1 &
                                                    !ais_data$last_row &
                                                    ((ais_data$distance_travelled <= 1 & ais_data$time_travelled > 60 * 5) |
                                                       ais_data$distance_travelled == 0 |
                                                       ais_data$speed_kmh < 0.001 |
                                                       ais_data$time_travelled == 0)])

  strange_speed <- sort(unique(c(strange_speed[!(strange_speed %in% short_time)], short_time)))

  ## avoid consecutive GPS detected (but not always actual) errors to be both removed: only the first error is removed
  strange_speed <- strange_speed[c(T, (strange_speed[-1] - strange_speed[-length(strange_speed)]) >= 2)]
  it_sp <- 0

  ## recalculate the time/speed/distance travelled for points consecutive to removed errors
  if (length(strange_speed) > 0) {
    to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]

    to_correct <- to_correct %>%
      dplyr::select(-(c(time_travelled, distance_travelled, speed_kmh))) %>%
      left_join(AIStravel(.,
                          crs_meters = crs_meters,
                          time_stop = time_stop,
                          mmsi_time_to_order = FALSE,
                          return_sf = FALSE,
                          return_3035_coords = FALSE) %>%
                  dplyr::select(time_travelled, distance_travelled, speed_kmh, mmsi, timestamp),
                by = c("mmsi", "timestamp"))

    ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
      dplyr::mutate(speed_kmh_corrected = F) %>%
      rbind(to_correct %>%
              dplyr::filter(id_ais_data_initial %in% (strange_speed + 1)) %>%
              dplyr::mutate(speed_kmh_corrected = T)) %>%
      dplyr::arrange(id_ais_data_initial)

    rm(to_correct)
    rm(strange_speed)

    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if ("real_high_speed" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      dplyr::mutate(high_speed = real_high_speed) %>%
      dplyr::select(-real_high_speed)
  }

  filt <- unique(c(init_cols, "speed_kmh_corrected", "time_travelled", "distance_travelled", "speed_kmh", "station", "high_speed"))
  filt <- filt[filt %in% colnames(ais_data)]

  ais_data <- ais_data %>%
    dplyr::select(dplyr::all_of(filt))

  return(ais_data)
}
