#' Interpolate all AIS data at the desired interval of time.
#'
#' Interpolate all AIS data at desired interval of time (but keep initial AIS data, so time interval are not equalized), after have corrected the GPS errors and delays (correcting the speed, distance and time travelled by the vessels), identified (and filter or not) the stations and aircraft. Contrary to AISinterpolate_all, you do not interpolate at desired times but instead, you select here the maximal interval of time that can occur in the AIS data between each AIS signal. If any interval is higher in the input data, an AIS data is interpolated at time_1 + maximum interval.
#'
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param crs_meters projection (crs) in 'meters' to use to calculate distance over the study area. Default to 3035 (ETRS89).
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param t_gap see "max_time_diff". Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction (otherwise other AIS data are filtered out).
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param correct_speed if TRUE, GPS errors and GPS delays are identified and removed from AIS data. Vessel speeds, distance and time travelled are corrected. Usually necessary.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions. The default expression has been tested as relevant and appropriate to filter GPS errors and delays, still checks are necessary.
#' @param filter_station if TRUE, filter the stations out.
#' @param filter_high_speed if TRUE, filter the aircraft out.
#' @param quantile_station Quantile (0 to 1) of distance, by mmsi, which is compared to threshold_distance_station to assess if the MMSI is a station or not: if below threshold_distance_station, MMSI is considered as stationary and is a station. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.
#' @param threshold_distance_station Threshold of distance (meters) used to assess if the MMSI is a station.
#' @param quantile_high_speed Quantile (0 to 1) of speed, by mmsi, which is compared to threshold_high_speed to assess if the MMSI is a aircraft or not: if above threshold_high_speed, MMSI is considered as a station. We used 0.97 to prevent misinterpretations from GPS errors.
#' @param threshold_high_speed Threshold of speed (km/h) used to assess if the MMSI is an aircraft.
#' @param interpolate_station if FALSE, do not interpolate the positions of the stations.
#' @param interpolate_high_speed if FALSE, do not interpolate the positions of the aircrafts.
#'
#' @return return AIS data with the columns:
#' \itemize{
#' \item id_ais_data_initial: identifier of the row in the ordered, corrected and cleaned ais data. Used for internal computation. For interpolated positions, id_ais_data_initial is the same than the next real existing AIS data.
#' \item station: if TRUE, the MMSI has been identified as a station.
#' \item high_speed: if TRUE, the MMSI has been identified as an high speed craft (specially used for aircraft).
#' \item any_NA_speed_kmh: if TRUE, at least one of the speeds of this MMSI has a speed as NA (so distance_travelled or time_travelled has a issue and the AIS data must be checked). Should not occur.
#' \item n_point_mmsi_initial_data: number of point of the MMSI in the AIS data after have removed the points with inexisting longitude and latitude.
#' \item id_mmsi_point_initial: identifier for the MMSI point after ordering, correcting and cleaning.
#' \item speed_kmh_corrected: if TRUE, the speed of the line has been corrected.
#' \item interpolated: if TRUE, this MMSI position has been interpolated.
#' \item time_travelled: number of seconds since the last reception or interpolation of an AIS signal (0 if first reception).
#' \item distance_travelled:  distance travelled (meters) since the last reception or interpolation of an AIS signal (0 if first reception).
#' \item speed_kmh: speed (km/h) of the vessels since the last reception or interpolation of an AIS signal.}
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
#' AISinterpolate_all(ais_data = ais,
#'                   mmsi_time_to_order = T,
#'                   t_gap = 60,
#'                   time_stop = 5 * 60 * 60,
#'                   correct_speed = T,
#'                   threshold_speed_to_correct = 100,
#'                   filter_station = T,
#'                   filter_high_speed = T,
#'                   interpolate_station = F,
#'                   interpolate_high_speed = F)}
#' @export

AISinterpolate_all <- function(ais_data,
                               crs_meters = 3035,
                               mmsi_time_to_order = TRUE,
                               t_gap = 30,
                               time_stop = 5 * 60 * 60,
                               correct_speed = TRUE,
                               threshold_speed_to_correct = 100,
                               threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 1], na.rm = T) +
                                                                                                sd(speed_kmh[speed_kmh > 1 & speed_kmh < quantile(speed_kmh[speed_kmh > 1], .75)]) * 5 + 15))},
                               filter_station = TRUE,
                               filter_high_speed = TRUE,
                               quantile_station = 0.975,
                               threshold_distance_station = 1,
                               quantile_high_speed = 0.97,
                               threshold_high_speed = 110,
                               interpolate_station = FALSE,
                               interpolate_high_speed = FALSE
                               ){

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))

  cat("This function is interpolating ALL vessel tracks with AIS gap > t_gaps: please consider using AISinterpolate_at if you are interested in just few timestamps or locations, or if you are interested in vessel tracks with equalised time-steps between each AIS signal (which wont be equalised with the present function - only interpolated in case time difference between two AIS locations is > t_gap)\n\n")

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n())

  ais_data <- ais_data %>%
    AISidentify_stations_aircraft(ais_data = .,
                                  quantile_station = quantile_station,
                                  threshold_distance_station = threshold_distance_station,
                                  quantile_high_speed = quantile_high_speed,
                                  threshold_high_speed = threshold_high_speed)

  if (filter_station) {
    lines <- which(ais_data$station)

    to_remove <- ais_data[lines,]

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }

    cat(length(lines), "data removed for STATION, i.e.", length(unique(to_remove$mmsi)), "stations\n")
    rm(to_remove)
  }
  if (filter_high_speed) {
    lines <- which(ais_data$high_speed)

    to_remove <- ais_data[lines,]

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }
    cat(length(lines), "data removed for HIGH SPEED, i.e.", length(unique(to_remove$mmsi)), "high speed engine\n")
    rm(to_remove)
  }

  gc()

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)
  }

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n()) %>%
    group_by(mmsi) %>%
    dplyr::mutate(id_mmsi_point_initial_real = id_mmsi_point_initial,
                  id_mmsi_point_initial = 1:n()) %>%
    ungroup()

  if (correct_speed) {
    cat("   --> Correct speeds\n")

    ais_data <- AIScorrect_speed(ais_data = ais_data,
                                 crs_meters = crs_meters,
                                 mmsi_time_to_order = F,
                                 correct_high_speed_craft = F,
                                 threshold_speed_to_correct = threshold_speed_to_correct,
                                 threshold_speed_to_correct_expr = threshold_speed_to_correct_expr,
                                 time_stop = time_stop,
                                 quantile_station = quantile_station,
                                 threshold_distance_station = threshold_distance_station,
                                 quantile_high_speed = quantile_high_speed,
                                 threshold_high_speed = threshold_high_speed)

  }

  to_interp <- ais_data %>%
    group_by(mmsi) %>%
    dplyr::mutate(idd = 1:n()) %>%
    dplyr::filter(idd != 1) %>%
    ungroup() %>%
    dplyr::select(-idd)

  to_interp <- to_interp[to_interp$time_travelled > 1.5*t_gap & to_interp$time_travelled < time_stop, ]

  if (!interpolate_station) {
    to_interp <- to_interp %>%
      dplyr::filter(!station)
  }
  if (!interpolate_high_speed) {
    to_interp <- to_interp %>%
      dplyr::filter(!high_speed)
  }

  prec <- ais_data[ais_data$id_ais_data_initial %in% (to_interp$id_ais_data_initial - 1), ]

  interp <- to_interp %>%
    dplyr::mutate(ttimestamp = prec$timestamp,
           tmmsi = prec$mmsi,
           tlon = prec$lon,
           tlat = prec$lat)

  rm(prec)

  interp <- interp[interp$mmsi == interp$tmmsi, ] %>%
    group_by(id_ais_data_initial) %>%
    dplyr::reframe(timestamp = c(seq(from = ttimestamp, to = timestamp - t_gap/2, by = t_gap), timestamp)[-1],
                   diffTime_interpolation = t_gap,
                   speed_kmh = unique(speed_kmh),
                   interpolated = c(rep(T, length(timestamp) - 1), F),
                   time_travelled = c(rep(t_gap, length(timestamp) - 1), last(timestamp) - timestamp[length(timestamp) - 1]),
                   distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                   lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
                   lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
    )

  interp_eez <- to_interp %>%
    dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat")) %>%
    left_join(interp, by = "id_ais_data_initial")

  rm(to_interp)
  rm(interp)

  ais_data <- ais_data %>%
    dplyr::filter(!(ais_data$id_ais_data_initial %in% unique(interp_eez$id_ais_data_initial))) %>%
    dplyr::mutate(diffTime_interpolation = t_gap,
                  interpolated = F) %>%
    rbind(interp_eez)

  ais_data <- AIStravel(ais_data = ais_data %>%
                     dplyr::select(-c(time_travelled, distance_travelled, speed_kmh)),
                   crs_meters = crs_meters,
                   time_stop = time_stop,
                   mmsi_time_to_order = T,
                   return_3035_coords = ifelse("X" %in% colnames(ais_data) & "Y" %in% colnames(ais_data),
                                               T,
                                               F))

  ais_data <- ais_data %>%
    group_by(mmsi) %>%
    dplyr::mutate(n_point_mmsi_final_data = n(),
                  id_mmsi_point_interp = 1:n(),
                  datetime = lubridate::as_datetime(timestamp),
                  date = as.character(lubridate::date(datetime))) %>%
    ungroup()

  rm(interp_eez)

  if ("id_mmsi_point_initial_real" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      mutate(id_mmsi_point_initial = id_mmsi_point_initial_real) %>%
      dplyr::select(-id_mmsi_point_initial_real)
  }

  return(ais_data)

}
