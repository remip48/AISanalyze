#' Interpolate all AIS data at the desired interval of time.
#'
#' Interpolate all AIS data to have AIS positions at the desired interval of time, after have corrected the GPS errors and delays (correcting the speed, distance and time travelled by the vessels), identified (and filter or not) the stations and aircraft
#'
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param t_gap see "max_time_diff". Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction (otherwise other AIS data are filtered out).
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param correct_speed if TRUE, GPS errors and GPS delays are identified and removed from AIS data. Vessel speeds, distance and time travelled are corrected. Usually necessary.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions. The default expression has been tested as relevant and appropriate to filter GPS errors and delays, but is long to compute. For very large AIS datasets, it could therefore be appropriate to modify it.
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
#' id_ais_data_initial: identifier of the row in the ordered, corrected and cleaned ais data. Used for internal computation. For interpolated positions, id_ais_data_initial is the same than the next real existing AIS data.
#' station: if TRUE, the MMSI has been identified as a station.
#' high_speed: if TRUE, the MMSI has been identified as an high speed craft (specially used for aircraft).
#' any_NA_speed_kmh: if TRUE, at least one of the speeds of this MMSI has a speed as NA (so distance_travelled or time_travelled has a issue and the AIS data must be checked). Should not occur.
#' n_point_mmsi_initial_data: number of point of the MMSI in the AIS data after have removed the points with inexisting longitude and latitude.
#' id_mmsi_point_initial: identifier for the MMSI point after ordering, correcting and cleaning.
#' speed_kmh_corrected: if TRUE, the speed of the line has been corrected.
#' interpolated: if TRUE, this MMSI position has been interpolated.
#' time_travelled: number of seconds since the last reception or interpolation of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception or interpolation of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessels since the last reception or interpolation of an AIS signal.
#'
#' @export
#'
#' @examples # to add
AISinterpolate_all <- function(ais_data,
                               mmsi_time_to_order = T,
                               t_gap = 30,
                               time_stop = 5 * 60 * 60,
                               correct_speed = T,
                               threshold_speed_to_correct = 100,
                               threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 0], na.rm = T) +
                                                                                                sd(speed_kmh[speed_kmh > 0 & speed_kmh < quantile(speed_kmh, 0.75, na.rm = T)])*2.5 + 15))},
                               filter_station = T,
                               filter_high_speed = T,
                               quantile_station = 0.975,
                               threshold_distance_station = 10,
                               quantile_high_speed = 0.97,
                               threshold_high_speed = 110,
                               interpolate_station = F,
                               interpolate_high_speed = F#,
                               # spatial_limit = NA,
                               # on_Land_analysis = F,
                               # land_sf_polygon = NA
                               ){

  # param spatial_limit sf polygon object of the area where outside points must be filtered out of the output. Not tested and might lead to few errors.
  # param on_Land_analysis sf polygon object of the countries to study the reliability of GPS positions and interpolations with an analysis of the paths travelled by mmsi on land. Not tested and might lead to few errors.
  # param land_sf_polygon if on_Land_analysis, sf polygon object for countries.


  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  if (!(all(c("X", "Y") %in% colnames(ais_data)))) {
    if (!("sf" %in% class(ais_data))) {
      ais_data <- ais_data %>%
        dplyr::mutate(tlon = lon,
                      tlat = lat) %>%
        st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }
    if (st_crs(ais_data)$input != "EPSG:3035") {
      ais_data <- ais_data %>%
        st_transform(crs = 3035)
    }

    coords_AIS <- ais_data %>%
      st_coordinates() %>%
      as.data.frame()

    ais_data <- ais_data %>%
      st_drop_geometry() %>%
      dplyr::mutate(X = coords_AIS[,1],
                    Y = coords_AIS[,2])

    rm(coords_AIS)
  }

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n())

  ais_data <- ais_data %>%
    AISidentify_stations_aircraft(ais_data = .,
                                  quantile_station = quantile_station,
                                  threshold_distance_station = threshold_distance_station,
                                  quantile_high_speed = quantile_high_speed,
                                  threshold_high_speed = threshold_high_speed)
    # group_by(mmsi) %>%
    # dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) < threshold_distance_station, T, F),
    #        high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) > threshold_high_speed, T, F),
    #        any_NA_speed_kmh = ifelse(any(is.na(speed_kmh)), T, F),
    #        n_point_mmsi_initial_data = n(),
    #        id_mmsi_point_initial = 1:n()
    # ) %>%
    # ungroup()

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

  # if (correct_speed) {
  #   cat("Correct speeds\n")
  #
  #   strange_speed <- ais_data %>%
  #     group_by(mmsi) %>%
  #     dplyr::mutate(threshold_strange_speed = median(speed_kmh[speed_kmh > 0], na.rm = T),
  #                   Q3 = quantile(speed_kmh, 0.75, na.rm = T),
  #                   threshold_strange_speed = (threshold_strange_speed +
  #                                                sd(speed_kmh[speed_kmh > 0 & speed_kmh < Q3])*2.5 + 15)
  #     ) %>%
  #     ungroup() %>%
  #     dplyr::filter(id_mmsi_point_initial != 1) %>%
  #     mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
  #                                             threshold_speed_to_correct,
  #                                             threshold_strange_speed))
  #
  #   strange_speed <- strange_speed$id_ais_data_initial[(strange_speed$speed_kmh > threshold_speed_to_correct |
  #                                                         strange_speed$speed_kmh > strange_speed$threshold_strange_speed) &
  #                                                        (strange_speed$time_travelled < time_stop &
  #                                                           !(strange_speed$high_speed))]
  #
  #   short_time <- ais_data$id_ais_data_initial[ais_data$time_travelled < 600 &
  #                                                !(ais_data$high_speed) &
  #                                                ais_data$id_mmsi_point_initial != 1 &
  #                                                ais_data$distance_travelled <= 2]
  #
  #   short_time <- na.omit(map_dbl(short_time, function(st) {
  #     length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st])
  #     return(ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time),
  #                   st,
  #                   NA))
  #   }))
  #
  #   strange_speed <- sort(unique(c(strange_speed, short_time)))
  #   it_sp <- 0
  #
  #   while (length(strange_speed) > 0 & it_sp < 1) {
  #     # print(it_sp)
  #     nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 & ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 & !ais_data$station[strange_speed - 1])
  #     if (length(nextp) > 0) {
  #       strange_speed[nextp] <- strange_speed[nextp] - 1
  #       strange_speed <- unique(strange_speed)
  #     }
  #
  #     nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 &
  #                      ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
  #                      !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
  #                      !ais_data$station[strange_speed - 1])
  #     it_strange_sp <- 0
  #
  #     while (length(nextp) > 0 & it_strange_sp < 10) {
  #       # cat(it_strange_sp, "      ", length(nextp), "\n")
  #       strange_speed <- sort(unique(c(strange_speed, strange_speed[nextp] - 1)))
  #       nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 &
  #                        ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
  #                        !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
  #                        !ais_data$station[strange_speed - 1])
  #       it_strange_sp <- it_strange_sp + 1
  #     }
  #
  #     to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]
  #
  #     mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]
  #
  #     to_correct <- to_correct %>%
  #       dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
  #       dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
  #                     time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
  #                     distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
  #                     speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
  #       ) %>%
  #       dplyr::select(-c("tmmsi"))
  #
  #     to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
  #       dplyr::mutate(speed_kmh_corrected = T)
  #
  #     ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
  #       dplyr::mutate(speed_kmh_corrected = F) %>%
  #       rbind(to_correct) %>%
  #       dplyr::arrange(id_ais_data_initial)
  #
  #     rm(to_correct)
  #     rm(strange_speed)
  #
  #     ais_data <- ais_data %>%
  #       dplyr::mutate(id_ais_data_initial = 1:n())
  #
  #     strange_speed <- ais_data %>%  # [(ais_data$speed_kmh > threshold_speed_to_correct | ais_data$speed_kmh >) &
  #       group_by(mmsi) %>%
  #       dplyr::mutate(threshold_strange_speed = median(speed_kmh[speed_kmh > 0], na.rm = T),
  #                     Q3 = quantile(speed_kmh, 0.75, na.rm = T),
  #                     threshold_strange_speed = (threshold_strange_speed +
  #                                                  sd(speed_kmh[speed_kmh > 0 & speed_kmh < Q3])*2.5 + 15)
  #       ) %>%
  #       ungroup() %>%
  #       dplyr::filter(id_mmsi_point_initial != 1) %>%
  #       mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
  #                                               threshold_speed_to_correct,
  #                                               threshold_strange_speed))
  #
  #     strange_speed <- strange_speed$id_ais_data_initial[(strange_speed$speed_kmh > threshold_speed_to_correct |
  #                                                           strange_speed$speed_kmh > strange_speed$threshold_strange_speed) &
  #                                                          (strange_speed$time_travelled < time_stop &
  #                                                             !(strange_speed$high_speed))]
  #
  #     short_time <- ais_data$id_ais_data_initial[ais_data$time_travelled < 600 &
  #                                                  !(ais_data$high_speed) &
  #                                                  ais_data$id_mmsi_point_initial != 1 &
  #                                                  ais_data$distance_travelled <= 2]
  #
  #     short_time <- na.omit(map_dbl(short_time, function(st) {
  #       length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st])
  #       return(ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time),
  #                     st,
  #                     NA))
  #     }))
  #
  #     strange_speed <- sort(unique(c(strange_speed, short_time)))
  #
  #     it_sp <- it_sp + 1
  #   }
  #
  #   if (length(strange_speed) > 0) {
  #     to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]
  #
  #     mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]
  #
  #     to_correct <- to_correct %>%
  #       dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
  #       dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
  #                     time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
  #                     distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
  #                     speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
  #       ) %>%
  #       dplyr::select(-c("tmmsi"))
  #
  #     to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
  #       dplyr::mutate(speed_kmh_corrected = T)
  #
  #     ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
  #       dplyr::mutate(speed_kmh_corrected = F) %>%
  #       rbind(to_correct) %>%
  #       dplyr::arrange(id_ais_data_initial)
  #
  #     rm(to_correct)
  #     rm(strange_speed)
  #
  #     ais_data <- ais_data %>%
  #       dplyr::mutate(id_ais_data_initial = 1:n())
  #   }
  #
  # }

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

  # if (!is.na(spatial_limit)) {
  #   to_keep <- colnames(interp_eez)
  #
  #   interp_eez <- interp_eez %>%
  #     dplyr::mutate(tlon = lon,
  #                   tlat = lat) %>%
  #     st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
  #     st_transform(crs = 3035)
  #
  #   interp_eez <- interp_eez[st_intersects(interp_eez, spatial_limit %>%
  #                                            st_transform(crs = 3035), sparse = F), ] %>%
  #     st_drop_geometry() %>%
  #     dplyr::select(all_of(to_keep))
  #
  #   rm(to_keep)
  # }

  ais_data <- ais_data %>%
    dplyr::filter(!(ais_data$id_ais_data_initial %in% unique(interp_eez$id_ais_data_initial))) %>%
    dplyr::mutate(diffTime_interpolation = t_gap,
                  interpolated = F) %>%
    rbind(interp_eez) %>%
    dplyr::arrange(mmsi, timestamp) %>%
    group_by(mmsi) %>%
    dplyr::mutate(n_point_mmsi_final_data = n(),
                  id_mmsi_point_interp = 1:n(),
                  datetime = lubridate::as_datetime(timestamp),
                  date = as.character(lubridate::date(datetime))) %>%
    ungroup()

  rm(interp_eez)

  # if (on_Land_analysis)  {
  #   ######### to check on land,
  #   ais_data <- ais_data %>%
  #     dplyr::mutate(tlon = lon,
  #                   tlat = lat) %>%
  #     st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
  #     st_transform(crs = 3035) %>%
  #     dplyr::mutate(on_Land = c(ifelse(st_intersects(., land_sf_polygon %>%
  #                                                      st_transform(crs = 3035), sparse = F) == F, F, T))) %>%
  #     st_drop_geometry()
  #
  #   ais_data <- ais_data %>%
  #     group_by(mmsi) %>%
  #     dplyr::mutate(sum_distance_mmsi_on_Land = sum(distance_travelled[on_Land == T], na.rm = T),
  #            total_distance_mmsi = sum(distance_travelled, na.rm = T),
  #            sum_time_mmsi_on_Land = sum(time_travelled[on_Land == T], na.rm = T),
  #            total_time_mmsi = sum(time_travelled, na.rm = T)) %>%
  #     ungroup() %>%
  #     group_by(mmsi, id_mmsi_point_initial) %>%
  #     dplyr::mutate(sum_distance_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
  #            sum_time_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(time_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
  #            sum_distance_interp_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[interpolated == T], na.rm = T), NA),
  #            sum_time_interp_for_initial_points = ifelse(any(interpolated), sum(time_travelled[interpolated == T], na.rm = T), NA),
  #            perc_distance_interp_for_initial_points = dist_interp_idd_on_Land / dist_interp_idd,
  #            perc_time_interp_for_initial_points = time_interp_idd_on_Land / time_interp_idd) %>%
  #     ungroup()
  # }

  if ("id_mmsi_point_initial_real" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      mutate(id_mmsi_point_initial = id_mmsi_point_initial_real) %>%
      dplyr::select(-id_mmsi_point_initial_real)
  }

  return(ais_data)

}
