#' Extract AIS positions around target locations and times
#'
#' Unlike [AISextract()], this function can return multiple positions for the
#' same vessel.
#'
#' @param data Data frame containing `timestamp`, `lon`, and `lat`.
#'   `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
#'   and `lat` must be numeric.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp`, `lon`, and `lat` must be numeric. Another vessel
#'   identifier may be used if the column is named `mmsi`.
#' @param search_into_radius_m Search radius (m).
#' @param interval_time_before Time window (s) before each `data$timestamp`.
#' @param interval_time_after Time window (s) after each `data$timestamp`.
#'
#' @return The input data joined with all AIS positions matching the specified
#' time window and search radius. Input rows are duplicated when multiple AIS
#' positions are found. If no AIS position matches, AIS columns are filled with
#' `NA`. Additional columns include:
#' \itemize{
#' \item `distance_effort_ais_m`: Distance (m) between the input location and the AIS position.
#' \item `timestamp_AIS_to_extract`: Target extraction timestamp.
#' \item `diffTime_AIS_extraction_effort`: Difference (s) between the target and original timestamps.
#' \item `datetime_AIS_to_extract`: Datetime corresponding to `timestamp_AIS_to_extract`.
#' \item `diffTime_AIS_effort`: Difference (s) between the AIS timestamp and the original timestamp.
#' }
#'
#' @examples
#' \dontrun{
#' data("ais")
#' data("point_to_extract")
#'
#' library(dplyr)
#' library(lubridate)
#' point_to_extract <- point_to_extract %>%
#'   mutate(timestamp = as.numeric(ymd_hm(datetime)))
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             time_stop = 5*3600,
#'             mmsi_time_to_order = T,
#'             return_sf = F,
#'             return_3035_coords = F) %>%
#'   AISinterpolate_at(data = point_to_extract,
#'                   ais_data = .,
#'                   mmsi_time_to_order = T,
#'                   load_existing_files = T,
#'                   save_AISinterlate_at = T,
#'                   overwrite = F,
#'                   file_AISinterlate_at = "AISinterpolate_at",
#'                   radius = 200000,
#'                   time_stop = 5*3600,
#'                   correct_speed = T,
#'                   threshold_speed_to_correct = 100,
#'                   filter_station = T,
#'                   filter_high_speed = T,
#'                   parallelize = F)
#'
#' AISextract_all(data = point_to_extract,
#'            ais_data = ais,
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)}
#' @export

AISextract_all <- function(data,
                           crs_meters = 3035,
                           ais_data,
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

  ais_data <- ais_data[ais_data$timestamp >= (min(data$timestamp,
                                                  na.rm = T) - (interval_time_before)) & ais_data$timestamp <=
                         (max(data$timestamp, na.rm = T) + interval_time_after),
  ]

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters,
                                     coordinates_to_write = c("ais_X", "ais_Y")) %>%
    st_drop_geometry()

  ais_data <- rename_colums_ais(ais_data,
                                data)

  data <- rename_columns_data(data)

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  data <- data %>%
    dplyr::mutate(idd_effort = 1:n())

  return_columns <- function(.x) {
    return(.x)
  }

  ais_data <- ais_data %>%
    as.data.frame() %>%
    dplyr::rename(ais_timestamp = timestamp)

  time_ais <- map_dfr(unique(data$timestamp), function(dt) {
    eff_dt <- data[data$timestamp == dt, ]
    mmsi_ref <- ais_data[ais_data$ais_timestamp >= (dt -
                                                      interval_time_before) & ais_data$ais_timestamp <=
                           (dt + interval_time_after) & ais_data$ais_X >= (min(eff_dt$X) -
                                                                             search_into_radius_m) & ais_data$ais_X <= (max(eff_dt$X) +
                                                                                                                          search_into_radius_m) & ais_data$ais_Y >= (min(eff_dt$Y) -
                                                                                                                                                                       search_into_radius_m) & ais_data$ais_Y <= (max(eff_dt$Y) +
                                                                                                                                                                                                                    search_into_radius_m), ]
    if (nrow(mmsi_ref) > 1) {
      out <- eff_dt %>% as.data.frame() %>% group_by(idd_effort) %>%
        dplyr::reframe(across(colnames(eff_dt)[colnames(eff_dt) !=
                                                 "idd_effort"], ~ return_columns(.x)), mmsi_ref %>%
                         dplyr::mutate(distance_effort_ais_m = sqrt((ais_X -
                                                                       X)^2 + (ais_Y - Y)^2))) %>% dplyr::filter(distance_effort_ais_m <=
                                                                                                                   search_into_radius_m)
    }
    else {
      out <- eff_dt
    }
    rm(eff_dt)
    rm(mmsi_ref)
    return(out)
  })

  time_ais <- check_process_extraction(time_ais) %>%
    dplyr::mutate(diffTime_AIS_effort = ais_timestamp -
                    timestamp) %>%
    dplyr::select(-c("point")[c("point") %in% colnames(time_ais)])

  if (any(!(data$idd_effort %in% time_ais$idd_effort))) {
    time_ais <- map_dfr(list(time_ais, data[!(data$idd_effort %in%
                                                time_ais$idd_effort), ]), function(l) {
                                                  return(l)
                                                })
  }

  gc()

  time_ais <- time_ais %>%
    dplyr::select(!c("idd_effort")) %>%
    dplyr::select(all_of(colnames(ais_data)[colnames(ais_data) %in% colnames(.)]),
                  all_of(colnames(.)[!(colnames(.) %in% c(colnames(data),
                                                          colnames(ais_data)))]),
                  all_of(all_of(colnames(data)[colnames(data) %in% colnames(.)])))

  rm(data)

  return(time_ais)
}
