#' Identify AIS base stations and high-speed craft
#'
#' Stations and aircraft are identified from speed, distance and time only.
#' Other criteria (e.g. MMSIs with fewer than 9 digits) are not considered.
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric. Another vessel identifier may be used if
#'   the column is named `mmsi`.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#'
#' @return The input AIS data with the following additional columns:
#' \itemize{
#' \item `station`: Whether the MMSI is classified as a base station.
#' \item `high_speed`: Whether the MMSI is classified as a high-speed craft.
#' \item `n_point_mmsi_initial_data`: Number of valid AIS positions for the MMSI.
#' \item `id_mmsi_point_initial`: Sequential identifier of each AIS position.
#' }
#'
#' @examples
#' library(AISanalyze)
#' data("ais")
#'
#' # Define the Unix time (seconds since 1970-01-01)
#' ais$timestamp <- as.numeric(lubridate::ymd_hms(ais$datetime))
#'
#' # calculate the travelled distance, time, and speed:
#' ais <- AIStravel(ais_data = ais)
#'
#' # Identify stations and aircrafts:
#' out <- AISidentify_stations_aircraft(ais_data = ais)
#' @export

AISidentify_stations_aircraft <- function(ais_data,
                                          crs_meters = 3035) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))
  assertthat::assert_that("time_travelled" %in% colnames(ais_data) & "distance_travelled" %in% colnames(ais_data) & "speed_kmh" %in% colnames(ais_data),
                          msg = "Please first run AIStravel() to calculate speed, distance and time travelled.")

  init_cols <- colnames(ais_data)

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(stats::quantile(distance_travelled, 0.975, na.rm = TRUE) <= 1 | stats::quantile(speed_kmh, 0.975, na.rm = TRUE) <= 0.01, TRUE, FALSE),
                  high_speed = ifelse(stats::quantile(speed_kmh, 1 - 0.97, na.rm = TRUE) >= 110, TRUE, FALSE),
                  n_point_mmsi_initial_data = dplyr::n(),
                  id_mmsi_point_initial = 1:dplyr::n()) %>%
    dplyr::ungroup()

  filt <- unique(c(init_cols, "time_travelled", "distance_travelled", "speed_kmh", "station", "high_speed", "n_point_mmsi_initial_data", "id_mmsi_point_initial"))
  filt <- filt[filt %in% colnames(ais_data)]

  return(ais_data %>%
           dplyr::select(dplyr::all_of(filt)))
}
