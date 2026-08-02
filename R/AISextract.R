#' Extract AIS positions around target locations and times
#'
#' Returns either (depending on `return_all_vessel_locations`):
#'  \itemize{
#'   \item each vessel position at the target timestamps.
#'   \item or all vessel positions within a specified time window.
#'  }
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp`, `lon`, and `lat` must be numeric. Another vessel
#'   identifier may be used if the column is named `mmsi`.
#' @param data Data frame containing `timestamp`, `lon`, and `lat`.
#'   `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
#'   and `lat` must be numeric.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param return_all_vessel_locations Logical. If `TRUE`, returns all vessel
#'   positions within the specified time window. Otherwise, returns only the
#'   closest position in time.
#' @param search_into_radius_m Search radius (m).
#' @param search_shape `"circle"` (default; selects vessels within
#'   `search_into_radius_m` of the target location) or `"square"` (selects
#'   vessels within `search_into_radius_m` in both the X and Y directions,
#'   useful for grid-based analyses).
#' @param interval_time_before Time window (s) before each `data$timestamp`.
#' @param interval_time_after Time window (s) after each `data$timestamp`.
#' @param nb_cores Number of CPU cores used.
#' @param outfile File used to save logs.
#'
#' @return `data` joined with matching AIS positions. Rows are duplicated when
#' several vessel positions match a target location and time. If no vessel is
#' found, AIS columns (including `mmsi`) are filled with `NA`. The output also
#' includes `distance_vessel_to_location_m`, the distance (m) between the
#' target location and vessel positions.
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#' data("point_to_extract")
#'
#' point_to_extract$timestamp <- as.numeric(lubridate::ymd_hm(point_to_extract$datetime))
#'
#' ais <- ais %>%
#'   dplyr::mutate(timestamp = as.numeric(lubridate::ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .) %>%
#'   AISinterpolate(ais_data = .,
#'            type_interpolation = "exact_timestamp",
#'            exact_timestamp = list(
#'              timestamp_to_interpolate = point_to_extract$timestamp,
#'              locations_of_interest = data.frame(lon = point_to_extract$lon,
#'                                                 lat = point_to_extract$lat),
#'              radius = 200000),
#'            crs_meters = 3035)
#'
#' # to return all vessel positions around the target location/timestamps:
#' out <- AISextract(ais_data = ais,
#'            data = point_to_extract,
#'            crs_meters = 3035,
#'            return_all_vessel_locations = TRUE,
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)
#'
#' # to return the position of each vessel closest in time to the target
#' # timestamps (around the target location)
#' out <- AISextract(ais_data = ais,
#'            data = point_to_extract,
#'            crs_meters = 3035,
#'            return_all_vessel_locations = FALSE,
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)
#'            }
#' @export

AISextract <- function(ais_data,
                       data,
                       crs_meters = 3035,
                       return_all_vessel_locations = T,
                       search_into_radius_m = 50000,
                       search_shape = "circle",
                       interval_time_before = 5 * 60,
                       interval_time_after = 5 * 60,
                       nb_cores = 1,
                       outfile = "log.txt")
{

  assertthat::assert_that(search_shape %in% c("circle", "square"))
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
  assertthat::assert_that("time_travelled" %in% colnames(ais_data) & "distance_travelled" %in% colnames(ais_data) & "speed_kmh" %in% colnames(ais_data),
                          msg = "Please first run AIStravel() to calculate speed, distance and time travelled.")

  cat(ifelse(return_all_vessel_locations,
             "Returning all vessel positions within [t - interval_time_before, t + interval_time_after]. Set `return_all_vessel_locations = FALSE` to return only the closest in time position.\n",
             "Returning only the vessel position closest in time within [t - interval_time_before, t + interval_time_after]. Set `return_all_vessel_locations = TRUE` to return all matching positions.\n"
  ))

  data <- data %>%
    rename_columns_data(.) %>%
    add_coordinates_meters(., crs_meters = crs_meters) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(idd_effort = 1:dplyr::n())

  ais_data <- ais_data[ais_data$timestamp >= (min(data$timestamp, na.rm = T) - (interval_time_before)) &
                         ais_data$timestamp <= (max(data$timestamp, na.rm = T) + interval_time_after), ] %>%
    add_coordinates_meters(.,
                           crs_meters = crs_meters,
                           coordinates_to_write = c("ais_X", "ais_Y")) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(ais_X >= (min(data$X) - search_into_radius_m)) %>%
    dplyr::filter(ais_X <= (max(data$X) + search_into_radius_m)) %>%
    dplyr::filter(ais_Y >= (min(data$Y) - search_into_radius_m)) %>%
    dplyr::filter(ais_Y <= (max(data$Y) + search_into_radius_m)) %>%
    rename_colums_ais(.,
                    data)  %>%
    dplyr::rename(ais_timestamp = timestamp)%>%
    as.data.frame()

  init_cols <- colnames(ais_data)

  if (nrow(ais_data) > 0) {
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

    extracted_ais <- foreach::foreach(ais_data_core = ais_data,
                                      # .export = c(),
                                      .noexport = c("assign_mmsi_to_core", "ais_data"),
                                      .packages = c("dplyr", "purrr")
    ) %dopar% {

      purrr::map_dfr(unique(data$timestamp), function(dt) {
        eff_dt <- data[data$timestamp == dt, ]

        mmsi_ref <- ais_data_core[ais_data_core$ais_timestamp >= (dt - interval_time_before) &
                                    ais_data_core$ais_timestamp <= (dt + interval_time_after) &
                                    ais_data_core$ais_X >= (min(eff_dt$X) - search_into_radius_m) &
                                    ais_data_core$ais_X <= (max(eff_dt$X) + search_into_radius_m) &
                                    ais_data_core$ais_Y >= (min(eff_dt$Y) - search_into_radius_m) &
                                    ais_data_core$ais_Y <= (max(eff_dt$Y) + search_into_radius_m), ]

        if (nrow(mmsi_ref) >= 1) {

          if (!return_all_vessel_locations) {
            mmsi_ref_infos <- mmsi_ref %>%
              dplyr::mutate(idd_ais = 1:dplyr::n())

            mmsi_ref <- mmsi_ref_infos %>%
              as.data.frame() %>%
              dplyr::group_by(mmsi) %>%
              dplyr::reframe(point = which.min(abs(ais_timestamp - dt)),
                             idd_ais = idd_ais[point],
                             ais_X = ais_X[point],
                             ais_Y = ais_Y[point],
                             ais_timestamp = ais_timestamp[point])
          }

          out <- eff_dt %>%
            as.data.frame() %>%
            dplyr::group_by(idd_effort) %>%
            dplyr::reframe(mmsi_ref %>%
                             dplyr::filter(abs(ais_X - X) <= search_into_radius_m) %>%
                             dplyr::filter(abs(ais_Y - Y) <= search_into_radius_m) %>%
                             dplyr::mutate(distance_vessel_to_location_m = sqrt((ais_X - X)^2 + (ais_Y - Y)^2)))

          if (search_shape == "circle") {
            out <- out %>%
              dplyr::filter(distance_vessel_to_location_m <= search_into_radius_m)
          }

          out <- out %>%
            dplyr::left_join(eff_dt, by = "idd_effort")

          if (!return_all_vessel_locations) {
            out <- out %>%
              dplyr::left_join(mmsi_ref_infos %>%
                                 dplyr::select(-c(ais_X, ais_Y, mmsi, ais_timestamp)), by = "idd_ais") %>%
              dplyr::select(-c(idd_ais, point))
          }

        } else {
          out <- eff_dt
        }

        return(out)
      })
    }

    parallel::stopCluster(cl)
    gc()

    extracted_ais <- purrr::map_dfr(extracted_ais, rbind)
  } else {
    extracted_ais <- data
  }

  if (!("ais_timestamp" %in% colnames(extracted_ais))) {
    cat("\nNo AIS data extracted at all for the input data\n")
    extracted_ais <- extracted_ais %>%
      dplyr::mutate(mmsi = NA,
                    ais_timestamp = NA,
                    distance_vessel_to_location_m = NA)
  }

  if (any(!(data$idd_effort %in% extracted_ais$idd_effort))) {
    extracted_ais <- purrr::map_dfr(list(extracted_ais, data[!(data$idd_effort %in% extracted_ais$idd_effort), ]),
                        function(l) {return(l)})
  }

  return(extracted_ais %>%
           dplyr::select(!(c("idd_effort", "ais_X", "ais_Y")[c("idd_effort", "ais_X", "ais_Y") %in% colnames(.)])) %>%
           dplyr::select(dplyr::all_of(c(colnames(data)[colnames(data) %in% colnames(.)],
                                         "distance_vessel_to_location_m",
                                         init_cols[init_cols %in% colnames(.)]))) %>%
           dplyr::arrange(timestamp, ais_timestamp))
}
