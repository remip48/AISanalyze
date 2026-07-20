#' Extract AIS positions around target locations and times
#'
#' Returns either the vessel position closest in time to each target timestamp
#' or all vessel positions within a specified time window.
#'
#' @param data Data frame containing `timestamp`, `lon`, and `lat`.
#'   `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
#'   and `lat` must be numeric.
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp`, `lon`, and `lat` must be numeric. Another vessel
#'   identifier may be used if the column is named `mmsi`.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param return_all_vessel_locations Logical. If `TRUE`, returns all vessel
#'   positions within the specified time window. Otherwise, returns only the
#'   closest position in time.
#' @param search_into_radius_m Search radius (m).
#' @param interval_time_before Time window (s) before each `data$timestamp`.
#' @param interval_time_after Time window (s) after each `data$timestamp`.
#'
#' @return `data` joined with matching AIS positions. Rows are duplicated when
#' several vessel positions match a target location and time. If no vessel is
#' found, AIS columns (including `mmsi`) are filled with `NA`. The output also
#' includes `distance_vessel_to_location_m`, the distance (m) between the
#' target location and each vessel position.
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#' data("point_to_extract")
#'
#' point_to_extract <- point_to_extract %>%
#'   mutate(timestamp = as.numeric(ymd_hm(datetime)))
#'
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .) %>%
#'   AISinterpolate(ais_data = .,
#'            type_interpolation = "exact_timestamp",
#'            exact_timestamp = list(
#'              timestamp_to_interpolate = point_to_extract$timestamp,
#'              locations_of_interest = data.frame(lon = point_to_extract$lon,
#'                                                 lat = point_to_extract$lat),
#'              radius = 200000),
#'            crs_meters = 3035,
#'            parallelize = F)
#'
#' # to return all vessel positions around the target location/timestamps:
#' out <- AISextract(data = point_to_extract,
#'            ais_data = ais,
#'            crs_meters = 3035,
#'            return_all_vessel_locations = T
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)
#'
#' # to return the position of each vessel closest in time to the target
#' timestamps (around the target location)
#' out <- AISextract(data = point_to_extract,
#'            ais_data = ais,
#'            crs_meters = 3035,
#'            return_all_vessel_locations = T
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)}
#' @export

AISextract <- function(data,
                       ais_data,
                       crs_meters = 3035,
                       return_all_vessel_locations = T,
                       search_into_radius_m = 50000,
                       interval_time_before = 5 * 60,
                       interval_time_after = 5 * 60)
{

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(data$lon))
  assertthat::assert_that(is.numeric(data$lat))
  assertthat::assert_that(is.numeric(data$timestamp))
  assertthat::assert_that(is.numeric(search_into_radius_m))
  assertthat::assert_that(is.numeric(interval_time_before))
  assertthat::assert_that(is.numeric(interval_time_after))
  assertthat::assert_that(is.logical(return_all_vessel_locations))

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    stop("Please run AIStravel() before AISextract()")
  }

  cat(ifelse(
    return_all_vessel_locations,
    "Returning all vessel positions within [t - interval_time_before, t + interval_time_after]. Set `return_all_vessel_locations = FALSE` to return only the closest in time position.\n",
    "Returning only the vessel position closest in time within [t - interval_time_before, t + interval_time_after]. Set `return_all_vessel_locations = TRUE` to return all matching positions.\n"
  ))

  ais_data <- ais_data[ais_data$timestamp >= (min(data$timestamp, na.rm = T) - (interval_time_before)) &
                         ais_data$timestamp <= (max(data$timestamp, na.rm = T) + interval_time_after), ]

  ais_data <- add_coordinates_meters(ais_data,
                                     crs_meters = crs_meters,
                                     coordinates_to_write = c("ais_X", "ais_Y")) %>%
    sf::st_drop_geometry()

  ais_data <- rename_colums_ais(ais_data,
                                data)

  data <- rename_columns_data(data)

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry()

  data <- data %>%
    dplyr::mutate(idd_effort = 1:dplyr::n())

  ais_data <- ais_data %>%
    as.data.frame() %>%
    dplyr::rename(ais_timestamp = timestamp)

  time_ais <- purrr::map_dfr(unique(data$timestamp), function(dt) {

    eff_dt <- data[data$timestamp == dt, ]

    mmsi_ref <- ais_data[ais_data$ais_timestamp >= (dt - interval_time_before) &
                           ais_data$ais_timestamp <= (dt + interval_time_after) &
                           ais_data$ais_X >= (min(eff_dt$X) - search_into_radius_m) &
                           ais_data$ais_X <= (max(eff_dt$X) + search_into_radius_m) &
                           ais_data$ais_Y >= (min(eff_dt$Y) - search_into_radius_m) &
                           ais_data$ais_Y <= (max(eff_dt$Y) + search_into_radius_m), ]

    if (nrow(mmsi_ref) >= 1 & !return_all_vessel_locations) {

      mmsi_refi <- mmsi_ref %>%
        dplyr::mutate(idd_ais = 1:dplyr::n())

      mmsi_ref <- mmsi_refi %>%
        as.data.frame() %>%
        dplyr::group_by(mmsi) %>%
        dplyr::reframe(point = which.min(abs(ais_timestamp - dt)),
                       idd_ais = idd_ais[point],
                       ais_X = ais_X[point],
                       ais_Y = ais_Y[point],
                       ais_timestamp = ais_timestamp[point]
        )

    }
    if (nrow(mmsi_ref) >= 1) {

      out <- eff_dt %>%
        as.data.frame() %>%
        dplyr::group_by(idd_effort) %>%
        dplyr::reframe(mmsi_ref %>%
                         dplyr::mutate(distance_vessel_to_location_m = sqrt((ais_X - X)^2 + (ais_Y - Y)^2))) %>%
        dplyr::filter(distance_vessel_to_location_m <= search_into_radius_m) %>%
        dplyr::left_join(eff_dt, by = "idd_effort")

      if (!return_all_vessel_locations) {
        out <- out %>%
          dplyr::left_join(mmsi_refi %>%
                      dplyr::select(-c(ais_X, ais_Y, mmsi, ais_timestamp)), by = "idd_ais") %>%
          dplyr::select(-c(idd_ais, point))
      }

    }
    else {
      out <- eff_dt
    }

    rm(eff_dt)
    rm(mmsi_ref)

    return(out)
  })

  if (!("ais_timestamp" %in% colnames(time_ais))) {
    cat("\nNo AIS data extracted at all for the input data\n")
    time_ais <- time_ais %>%
      dplyr::mutate(mmsi = NA,
                    ais_timestamp = NA)
  }

  ## check no missing data in the output
  if (any(!(data$idd_effort %in% time_ais$idd_effort))) {
    time_ais <- purrr::map_dfr(list(time_ais,
                             data[!(data$idd_effort %in% time_ais$idd_effort), ]),
                        function(l) {return(l)})
  }

  gc()

  time_ais <- time_ais %>%
    dplyr::select(!c("idd_effort", "ais_X", "ais_Y")) %>%
    dplyr::select(dplyr::all_of(colnames(data)[colnames(data) %in% colnames(.)]),
                  dplyr::all_of(colnames(.)[!(colnames(.) %in% c(colnames(data),
                                                          colnames(ais_data)))]),
                  dplyr::all_of(colnames(ais_data)[colnames(ais_data) %in% colnames(.)])
                  ) %>%
    dplyr::arrange(timestamp, ais_timestamp)

  rm(data)

  return(time_ais)
}
