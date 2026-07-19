#' Calculate vessel travel metrics
#'
#' Calculates the distance travelled (m), travel time (s), and speed (km/h)
#' between consecutive AIS positions for each vessel (`mmsi`).
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric. Another vessel identifier may be used if
#'   the column is named `mmsi`.
#' @param crs_meters CRS (in metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param return_sf Logical. If `TRUE`, returns an `sf` object; otherwise,
#'   returns a data frame.
#' @param return_meter_coords Logical. If `TRUE`, adds
#'   coordinates (`X` and `Y`) in `crs_meters` to the output.
#'
#' @return The input AIS data with the following additional columns:
#' \itemize{
#' \item `time_travelled`: Travel time (s) since the previous AIS position.
#' \item `distance_travelled`: Distance travelled (m) since the previous AIS position.
#' \item `speed_kmh`: Vessel speed (km/h).
#' \item `X`, `Y`: coordinates in `crs_meters` when `return_meter_coords = TRUE`.
#' }
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#'
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime)))
#'
#' out <- AIStravel(ais_data = ais,
#'           return_sf = F,
#'           return_meter_coords = F)}
#' @export

AIStravel <- function(ais_data,
                      crs_meters = 3035,
                      return_sf = FALSE,
                      return_meter_coords = FALSE
) {
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.logical(return_sf))
  assertthat::assert_that(is.logical(return_meter_coords))

  ais_data <- ais_data %>%
    dplyr::arrange(mmsi, timestamp)

  ais_data <- ais_data[!duplicated(paste0(ais_data$mmsi, ais_data$timestamp)), ] %>%
    add_coordinates_meters(., crs_meters = crs_meters)

  if (!return_sf) {
    ais_data <- ais_data %>%
      sf::st_drop_geometry()
  }

  mmsi_prev <- ais_data$mmsi[-nrow(ais_data)]

  ais_data <- ais_data %>%
    dplyr::mutate(
      tmmsi = c("initial", mmsi_prev))

  ais_data <- ais_data %>%
    dplyr::mutate(time_travelled = timestamp - c(dplyr::first(timestamp), timestamp[-n()]),
                  time_travelled = ifelse(time_travelled > 5*60*60, 0,
                                          ifelse(mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)),
                                                 0,
                                                 time_travelled)),
                  distance_travelled = ifelse(time_travelled == 0 | is.na(time_travelled), 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                  speed_kmh = ifelse(is.na(time_travelled), 0,
                                     ifelse(time_travelled == 0,
                                            0,
                                            c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1]))))
    ) %>%
    dplyr::select(-c("tmmsi"))

  if (!return_meter_coords) {
    ais_data <- ais_data %>%
      dplyr::select(-c("X", "Y"))
  }

  rm(mmsi_prev)

  return(ais_data)

}
