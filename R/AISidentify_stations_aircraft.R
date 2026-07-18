#' Identify AIS base stations and high-speed craft
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric. Another vessel identifier may be used if
#'   the column is named `mmsi`.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param quantile_station Distance quantile (0-1) used to identify stationary
#'   MMSIs.
#' @param threshold_distance_station Distance threshold (m) for stationary
#'   MMSIs.
#' @param quantile_high_speed Speed quantile (0-1) used to identify high-speed
#'   craft.
#' @param threshold_high_speed Speed threshold (km/h) for high-speed craft.
#'
#' @return The input AIS data with the following additional columns:
#' \itemize{
#' \item `station`: Whether the MMSI is classified as a base station.
#' \item `high_speed`: Whether the MMSI is classified as a high-speed craft.
#' \item `any_NA_speed_kmh`: Whether any speed value is missing for the MMSI.
#' \item `n_point_mmsi_initial_data`: Number of valid AIS positions for the MMSI.
#' \item `id_mmsi_point_initial`: Sequential identifier of each AIS position.
#' }
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' library(dplyr)
#' library(lubridate)
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime)))

#' AISidentify_stations_aircraft(ais_data = ais)}
#' @export

AISidentify_stations_aircraft <- function(ais_data,
                                          crs_meters = 3035,
                                          quantile_station = 0.975,
                                          threshold_distance_station = 1,
                                          quantile_high_speed = 0.97,
                                          threshold_high_speed = 110) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))
  assertthat::assert_that(is.numeric(quantile_station))
  assertthat::assert_that(is.numeric(threshold_distance_station))
  assertthat::assert_that(is.numeric(quantile_high_speed))
  assertthat::assert_that(is.numeric(threshold_high_speed))

  warning("Stations and aircraft are only identified with speed, distance and time. Other criteria (e.g. less than 9 digits in the MMSI) are not used and should be filtered by the users itself if desired\n")

  init_cols <- colnames(ais_data)

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    ais_data <- AIStravel(ais_data = ais_data,
                          crs_meters = crs_meters,
                          time_stop = Inf,
                          mmsi_time_to_order = T,
                          return_sf = F,
                          return_3035_coords = T)
  }

  ais_data <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) <= threshold_distance_station, T, F),
                  high_speed = ifelse(quantile(speed_kmh, 1 - quantile_high_speed, na.rm = T) >= threshold_high_speed, T, F),
                  any_NA_speed_kmh = ifelse(any(is.na(speed_kmh)), T, F),
                  n_point_mmsi_initial_data = n(),
                  id_mmsi_point_initial = 1:n()) %>%
    ungroup()

  filt <- unique(c(init_cols, "time_travelled", "distance_travelled", "speed_kmh", "station", "high_speed", "any_NA_speed_kmh", "n_point_mmsi_initial_data",
                   "id_mmsi_point_initial"))
  filt <- filt[filt %in% colnames(ais_data)]

  ais_data <- ais_data %>%
    dplyr::select(dplyr::all_of(filt))

  return(ais_data)
}
