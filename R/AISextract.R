#' Extract the closest AIS position around target locations and times
#'
#' Unlike [AISextract_all()], this function returns at most one AIS position
#' per vessel per target timestamp.
#'
#' @param data Data frame containing `timestamp`, `lon`, and `lat`.
#'   `timestamp` must be Unix time (seconds since 1970-01-01), while `lon`
#'   and `lat` must be numeric.
#' @param crs_meters CRS (metres) used to calculate distances. Defaults to
#'   EPSG:3035.
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. Another vessel identifier may be used if the column is named
#'   `mmsi`.
#' @param search_into_radius_m Search radius (m).
#' @param duplicate_time Logical. If `TRUE`, duplicates timestamps backward in
#'   time.
#' @param max_time_diff Maximum time difference (s) used when
#'   `duplicate_time = TRUE`.
#' @param t_gap Time step (s) between duplicated timestamps. Also defines the
#'   search time window.
#' @param accelerate Logical. If `TRUE`, rounds timestamps to reduce
#'   computation time.
#' @param average_at Time resolution (s) used when `accelerate = TRUE`.
#'
#' @return The input data joined with the closest AIS position. Input rows are
#' duplicated when `duplicate_time = TRUE`. If no AIS position is found within
#' the search radius and time window, AIS columns are filled with `NA`. The
#' following columns are added:
#' \itemize{
#' \item `distance_effort_ais_m`: Distance (m) between the input and AIS positions.
#' \item `timestamp_AIS_to_extract`: Target extraction timestamp.
#' \item `diffTime_AIS_extraction_effort`: Difference (s) between the target and input timestamps.
#' \item `datetime_AIS_to_extract`: Datetime corresponding to `timestamp_AIS_to_extract`.
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
#' AISextract(data = point_to_extract,
#'            ais_data = ais,
#'            search_into_radius_m = 50000,
#'            duplicate_time = F,
#'            max_time_diff = 0,
#'            t_gap = 10*60,
#'            accelerate = F,
#'            average_at = 0)}
#' @export

AISextract <- function(data,
                       crs_meters = 3035,
                       ais_data,
                       search_into_radius_m = 50000,
                       duplicate_time = FALSE,
                       max_time_diff = 0,
                       t_gap = 10*60,
                       accelerate = F,
                       average_at = 0
) {

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(data$lon))
  assertthat::assert_that(is.numeric(data$lat))
  assertthat::assert_that(is.numeric(data$timestamp))

  ais_data <- ais_data[ais_data$timestamp > (min(data$timestamp, na.rm = T) - (max_time_diff + t_gap + average_at)) &
                         ais_data$timestamp < (max(data$timestamp, na.rm = T) + t_gap + average_at), ]

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters,
                                     coordinates_to_write = c("ais_X", "ais_Y")) %>%
    st_drop_geometry()

  ais_data <- rename_colums_ais(ais_data,
                                data)

  data <- rename_columns_data(data)

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  if (duplicate_time) {
    data <- DATAextend_time(data = data, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
  } else if (accelerate) {
    data <- DATAextend_time(data = data, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
  } else {
    data <- data %>%
      dplyr::mutate(timestamp_AIS_to_extract = timestamp,
                    diffTime_AIS_extraction_effort = 0,
                    datetime_AIS_to_extract = as.character(lubridate::as_datetime(timestamp_AIS_to_extract)))
  }

  data <- data %>%
    dplyr::mutate(timestamp_eff = timestamp) %>%
    dplyr::select(!"timestamp") %>%
    dplyr::mutate(idd_effort = 1:n())

  return_columns <- function(.x) {
    return(.x)
  }

  time_ais <- map_dfr(unique(data$timestamp_AIS_to_extract), function(dt) {

    eff_dt <- data[data$timestamp_AIS_to_extract == dt,]

    mmsi_ref <- ais_data[ais_data$timestamp > (dt - t_gap - average_at) &
                           ais_data$timestamp < (dt + t_gap + average_at) &
                           ais_data$ais_X >= (min(eff_dt$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_dt$X) + search_into_radius_m) &
                           ais_data$ais_Y >= (min(eff_dt$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_dt$Y) + search_into_radius_m),]

    if (nrow(mmsi_ref) >= 1) {
      mmsi_ref <- mmsi_ref %>%
        dplyr::mutate(idd_ais = 1:n())

      mmsi_eff <- mmsi_ref %>%
        as.data.frame() %>%
        group_by(mmsi) %>%
        dplyr::reframe(point = which.min(abs(timestamp - dt)),
                       idd_ais = idd_ais[point],
                       ais_X = ais_X[point],
                       ais_Y = ais_Y[point],
                       ais_timestamp = timestamp[point]
        )

      out <- eff_dt %>%
        as.data.frame() %>%
        group_by(idd_effort) %>%
        dplyr::reframe(
          mmsi_eff %>%
            dplyr::mutate(distance_effort_ais_m = sqrt((ais_X-X)^2 + (ais_Y-Y)^2))
        )

      out <- out[out$distance_effort_ais_m <= search_into_radius_m, ] %>%
        left_join(eff_dt, by = "idd_effort") %>%
        left_join(mmsi_ref %>%
                    dplyr::select(-c(ais_X, ais_Y, mmsi, timestamp)), by = "idd_ais") %>%
        dplyr::select(-idd_ais)

      rm(mmsi_eff)
    } else {
      out <- eff_dt
    }

    rm(eff_dt)
    rm(mmsi_ref)

    return(out)

  })

  time_ais <- check_process_extraction(time_ais) %>%
    dplyr::mutate(diffTime_AIS_extraction_effort = timestamp_AIS_to_extract - timestamp_eff) %>%
    dplyr::select(-c("point")[c("point") %in% colnames(time_ais)])

  if (any(!(data$idd_effort %in% time_ais$idd_effort))) {
    time_ais <- map_dfr(list(time_ais,
                             data[!(data$idd_effort %in% time_ais$idd_effort), ]),
                        function(l) {return(l)})
  }

  gc()

  time_ais <- time_ais %>%
    dplyr::mutate(timestamp = timestamp_eff)%>%
    dplyr::select(all_of(c(colnames(ais_data)[colnames(ais_data) %in% colnames(time_ais)],
                           colnames(time_ais)[!(colnames(time_ais) %in% c(colnames(data),
                                                          colnames(ais_data)))],
                           colnames(data)[colnames(data) %in% colnames(time_ais)]))) %>%
    dplyr::select(!c("timestamp_eff", "idd_effort"))

  rm(data)

  return(time_ais)

}
