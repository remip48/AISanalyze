#' Identify AIS base stations and high-speed craft
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
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#'
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(.)
#'
#' out <- AISidentify_stations_aircraft(ais_data = ais)}
#' @export

AISidentify_stations_aircraft <- function(ais_data,
                                          crs_meters = 3035) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(crs_meters))

  cat("Stations and aircraft are identified from speed, distance and time only. Other criteria (e.g. MMSIs with fewer than 9 digits) are not considered.\n")

  init_cols <- colnames(ais_data)

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry()

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    stop("Please run AIStravel() before AISidentify_stations_aircraft()")
  }

  ais_data <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(stats::quantile(distance_travelled, 0.975, na.rm = T) <= 1, T, F),
                  high_speed = ifelse(stats::quantile(speed_kmh, 1 - 0.97, na.rm = T) >= 110, T, F),
                  n_point_mmsi_initial_data = dplyr::n(),
                  id_mmsi_point_initial = 1:dplyr::n()) %>%
    dplyr::ungroup()

  filt <- unique(c(init_cols, "time_travelled", "distance_travelled", "speed_kmh", "station", "high_speed", "n_point_mmsi_initial_data",
                   "id_mmsi_point_initial"))
  filt <- filt[filt %in% colnames(ais_data)]

  ais_data <- ais_data %>%
    dplyr::select(dplyr::all_of(filt))

  return(ais_data)
}
