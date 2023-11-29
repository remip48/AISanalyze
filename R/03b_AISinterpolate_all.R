#' to interpolate AIS data for each mmsi
#'
#' @param ais_data AIS data. Must contain a column timestamp, lon, lat and mmsi (numeric value). the mmsi column is the identifier for vessel, and values can be replaced by the IMO for example, but the name of the column must be mmsi.
#' @param mmsi_time_to_order if MMSI and time are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution.
#' @param correct_speed if speeds of vessel need to be corrected, to remove GPS errors/delay and unrealistic speeds.
#' @param quantile_station Quantile of distance by mmsi used to assess if the mmsi is a station or not. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.
#' @param threshold_distance_station threshold of distance used to assess if the mmsi is a station.
#' @param quantile_high_speed Quantile of speed by mmsi used to assess if the mmsi is a aircraft or not. We used 0.975 to prevent misinterpretations from GPS errors.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_high_speed threshold of speed used to assess if the mmsi is an aircraft.
#' @param filter_station if the stations are filtered or not. mmsi_time_to_order
#' @param filter_high_speed if the aircraft are filtered or not.
#' @param interpolate_station if the stations are interpolated or not.
#' @param interpolate_high_speed if the aircraft are interpolated or not.
#' @param time_stop number of seconds around the AIS reception considered for interpolation of vessel positions. Interval of time higher than "time_stop" between 2 AIS receptions are considered as a stop of the movement. Filter also AIS data around data timestamp +- time_stop to accelerate the process.
#' @param t_gap interval of time (seconds) to which vessels positions are interpolated.
#' @param spatial_limit sf polygon object of the area where outside points must be filtered out of the output. Not tested and might lead to few errors.
#' @param on_Land_analysis sf polygon object of the countries to study the reliability of GPS positions and interpolations with an analysis of the paths travelled by mmsi on land. Not tested and might lead to few errors.
#' @param land_sf_polygon if on_Land_analysis, sf polygon object for countries.
#'
#' @return return AIS data with the columns:
#' id_ais_data_initial: identifier of the row line in the ais data, ordered, corrected and cleaned. Use for internal computation. For interpolated data, id_ais_data_initial is the same than the next real existing line.
#' station: if the MMSI is a station or not.
#' high_speed: if the MMSI is a high speed craft (used for aircraft) or not.
#' any_NA_speed_kmh: if any of the MMSI point has a value of speed of NA (so distance_travelled or time_travelled has a issue and the MMSI points must be checked). Should not occur.
#' n_point_mmsi_initial_data: number of point of the MMSI in the initial AIS data, removing firstly the inexisting longitude and latitude points.
#' id_mmsi_point_initial: identifier for the MMSI point in the ordered, corrected and cleaned AIS data.
#' speed_kmh_corrected: if the speed of this line has been corrected or not.
#' interpolated: if this AIS position is an interpolation or not.
#' @export
#'
#' @examples # to add
AISinterpolate_all <- function(ais_data,
                               mmsi_time_to_order = T,
                               correct_speed = T,
                               quantile_station = 0.975,
                               threshold_distance_station = 10,
                               quantile_high_speed = 0.97,
                               threshold_speed_to_correct = 100,
                               threshold_high_speed = 110,
                               filter_station = T,
                               filter_high_speed = T,
                               interpolate_station = F,
                               interpolate_high_speed = F,
                               time_stop = 5 * 60 * 60,
                               t_gap = 30,
                               spatial_limit = NA,
                               on_Land_analysis = F,
                               land_sf_polygon = NA){

  pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "uils", "stringr", "doParallel")
  inst <- which(!(pack %in% installed.packages()[,1]))

  if (length(inst) > 0) {
    lapply(pack[inst], function(p) {install.packages(p)})
  }

  lapply(pack, library, character.only = TRUE)

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
    dplyr::mutate(id_ais_data_initial = 1:n()) %>%
    group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) < threshold_distance_station, T, F),
           high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) > threshold_high_speed, T, F),
           any_NA_speed_kmh = ifelse(any(is.na(speed_kmh)), T, F),
           n_point_mmsi_initial_data = n(),
           id_mmsi_point_initial = 1:n()
    ) %>%
    ungroup()

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
    strange_speed <- ais_data %>%
      group_by(mmsi) %>%
      dplyr::mutate(threshold_strange_speed = median(speed_kmh[speed_kmh > 0], na.rm = T),
                    Q3 = quantile(speed_kmh, 0.75, na.rm = T),
                    threshold_strange_speed = (threshold_strange_speed +
                                                 sd(speed_kmh[speed_kmh > 0 & speed_kmh < Q3])*2.5 + 15)
      ) %>%
      ungroup() %>%
      dplyr::filter(id_mmsi_point_initial != 1) %>%
      mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                              threshold_speed_to_correct,
                                              threshold_strange_speed))

    strange_speed <- strange_speed$id_ais_data_initial[(strange_speed$speed_kmh > threshold_speed_to_correct |
                                                          strange_speed$speed_kmh > strange_speed$threshold_strange_speed) &
                                                         (strange_speed$time_travelled < time_stop &
                                                            !(strange_speed$high_speed))]

    short_time <- ais_data$id_ais_data_initial[ais_data$time_travelled < 600 &
                                                 !(ais_data$high_speed) &
                                                 ais_data$id_mmsi_point_initial != 1 &
                                                 ais_data$distance_travelled <= 2]

    short_time <- na.omit(map_dbl(short_time, function(st) {
      length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st])
      return(ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time),
                    st,
                    NA))
    }))

    strange_speed <- sort(unique(c(strange_speed, short_time)))
    it_sp <- 0

    while (length(strange_speed) > 0 & it_sp < 1) {
      # print(it_sp)
      nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 & ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 & !ais_data$station[strange_speed - 1])
      if (length(nextp) > 0) {
        strange_speed[nextp] <- strange_speed[nextp] - 1
        strange_speed <- unique(strange_speed)
      }

      nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 &
                       ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
                       !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
                       !ais_data$station[strange_speed - 1])
      it_strange_sp <- 0

      while (length(nextp) > 0 & it_strange_sp < 10) {
        # cat(it_strange_sp, "      ", length(nextp), "\n")
        strange_speed <- sort(unique(c(strange_speed, strange_speed[nextp] - 1)))
        nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .1 &
                         ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
                         !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
                         !ais_data$station[strange_speed - 1])
        it_strange_sp <- it_strange_sp + 1
      }

      to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]

      mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]

      to_correct <- to_correct %>%
        dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
        dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
                      time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                      distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                      speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
        ) %>%
        dplyr::select(-c("tmmsi"))

      to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
        dplyr::mutate(speed_kmh_corrected = T)

      ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
        dplyr::mutate(speed_kmh_corrected = F) %>%
        rbind(to_correct) %>%
        dplyr::arrange(id_ais_data_initial)

      rm(to_correct)
      rm(strange_speed)

      ais_data <- ais_data %>%
        dplyr::mutate(id_ais_data_initial = 1:n())

      strange_speed <- ais_data %>%  # [(ais_data$speed_kmh > threshold_speed_to_correct | ais_data$speed_kmh >) &
        group_by(mmsi) %>%
        dplyr::mutate(threshold_strange_speed = median(speed_kmh[speed_kmh > 0], na.rm = T),
                      Q3 = quantile(speed_kmh, 0.75, na.rm = T),
                      threshold_strange_speed = (threshold_strange_speed +
                                                   sd(speed_kmh[speed_kmh > 0 & speed_kmh < Q3])*2.5 + 15)
        ) %>%
        ungroup() %>%
        dplyr::filter(id_mmsi_point_initial != 1) %>%
        mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                threshold_speed_to_correct,
                                                threshold_strange_speed))

      strange_speed <- strange_speed$id_ais_data_initial[(strange_speed$speed_kmh > threshold_speed_to_correct |
                                                            strange_speed$speed_kmh > strange_speed$threshold_strange_speed) &
                                                           (strange_speed$time_travelled < time_stop &
                                                              !(strange_speed$high_speed))]

      short_time <- ais_data$id_ais_data_initial[ais_data$time_travelled < 600 &
                                                   !(ais_data$high_speed) &
                                                   ais_data$id_mmsi_point_initial != 1 &
                                                   ais_data$distance_travelled <= 2]

      short_time <- na.omit(map_dbl(short_time, function(st) {
        length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st])
        return(ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time),
                      st,
                      NA))
      }))

      strange_speed <- sort(unique(c(strange_speed, short_time)))

      it_sp <- it_sp + 1
    }

    if (length(strange_speed) > 0) {
      to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]

      mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]

      to_correct <- to_correct %>%
        dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
        dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
                      time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                      distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                      speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
        ) %>%
        dplyr::select(-c("tmmsi"))

      to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
        dplyr::mutate(speed_kmh_corrected = T)

      ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
        dplyr::mutate(speed_kmh_corrected = F) %>%
        rbind(to_correct) %>%
        dplyr::arrange(id_ais_data_initial)

      rm(to_correct)
      rm(strange_speed)

      ais_data <- ais_data %>%
        dplyr::mutate(id_ais_data_initial = 1:n())
    }

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
    dplyr::summarise(timestamp = c(seq(from = ttimestamp, to = timestamp - t_gap/2, by = t_gap), timestamp)[-1],
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

  if (!is.na(spatial_limit)) {
    to_keep <- colnames(interp_eez)

    interp_eez <- interp_eez %>%
      dplyr::mutate(tlon = lon,
                    tlat = lat) %>%
      st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
      st_transform(crs = 3035)

    interp_eez <- interp_eez[st_intersects(interp_eez, spatial_limit %>%
                                             st_transform(crs = 3035), sparse = F), ] %>%
      st_drop_geometry() %>%
      dplyr::select(all_of(to_keep))

    rm(to_keep)
  }

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

  if (on_Land_analysis)  {
    ######### to check on land,
    ais_data <- ais_data %>%
      dplyr::mutate(tlon = lon,
                    tlat = lat) %>%
      st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
      st_transform(crs = 3035) %>%
      dplyr::mutate(on_Land = c(ifelse(st_intersects(., land_sf_polygon %>%
                                                       st_transform(crs = 3035), sparse = F) == F, F, T))) %>%
      st_drop_geometry()

    ais_data <- ais_data %>%
      group_by(mmsi) %>%
      dplyr::mutate(sum_distance_mmsi_on_Land = sum(distance_travelled[on_Land == T], na.rm = T),
             total_distance_mmsi = sum(distance_travelled, na.rm = T),
             sum_time_mmsi_on_Land = sum(time_travelled[on_Land == T], na.rm = T),
             total_time_mmsi = sum(time_travelled, na.rm = T)) %>%
      ungroup() %>%
      group_by(mmsi, id_mmsi_point_initial) %>%
      dplyr::mutate(sum_distance_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
             sum_time_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(time_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
             sum_distance_interp_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[interpolated == T], na.rm = T), NA),
             sum_time_interp_for_initial_points = ifelse(any(interpolated), sum(time_travelled[interpolated == T], na.rm = T), NA),
             perc_distance_interp_for_initial_points = dist_interp_idd_on_Land / dist_interp_idd,
             perc_time_interp_for_initial_points = time_interp_idd_on_Land / time_interp_idd) %>%
      ungroup()
  }

  if ("id_mmsi_point_initial_real" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      mutate(id_mmsi_point_initial = id_mmsi_point_initial_real) %>%
      dplyr::select(-id_mmsi_point_initial_real)
  }

  return(ais_data)

}
