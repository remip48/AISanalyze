AIScorrect_speed <- function(ais_data,
                             mmsi_time_to_order = T,
                             correct_high_speed_craft = F,
                             threshold_speed_to_correct = 100,
                             threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 0], na.rm = T) +
                                                                                              sd(speed_kmh[speed_kmh > 0 & speed_kmh < quantile(speed_kmh, 0.75, na.rm = T)])*2.5 + 15))},
                             time_stop = 5*60*60,
                             nb_iteration = 1,
                             nb_iteration_successive_strange = 10
                             ) {

  init_cols <- colnames(ais_data)

  if (!(all(c("X", "Y") %in% colnames(ais_data)))) {
    if (!("sf" %in% class(ais_data))) {
      ais_data <- ais_data %>%
        dplyr::mutate(tlon = lon,
                      tlat = lat) %>%
        sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }
    if (st_crs(ais_data)$input != "EPSG:3035") {
      ais_data <- ais_data %>%
        sf::st_transform(crs = 3035)
    }

    coords_AIS <- ais_data %>%
      sf::st_coordinates() %>%
      as.data.frame()

    ais_data <- ais_data %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(X = coords_AIS[,1],
                    Y = coords_AIS[,2])

    rm(coords_AIS)
  }

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)
  }

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    ais_data <- AIStravel(ais_data = ais_data,
                          time_stop = time_stop,
                          mmsi_time_to_order = F,
                          return_sf = F,
                          return_3035_coords = T)
  }

  if (!("id_ais_data_initial" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if (!("id_mmsi_point_initial" %in% colnames(ais_data)) | !("station" %in% colnames(ais_data)) | !("high_speed" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) <= threshold_distance_station, T, F),
                    high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) >= threshold_high_speed, T, F),
                    id_mmsi_point_initial = 1:n()) %>%
      ungroup()
  }

  if (correct_high_speed_craft) {
    ais_data <- ais_data %>%
      dplyr::mutate(real_high_speed = high_speed,
                    high_speed = F)
  }

  strange_speed <- ais_data %>%  # [(ais_data$speed_kmh > threshold_speed_to_correct | ais_data$speed_kmh >) &
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr(speed_kmh)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(id_mmsi_point_initial != 1) %>%
    dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
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

  short_time <- na.omit(purrr::map_dbl(short_time, function(st) {
    length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st])
    return(ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time),
                  st,
                  NA))
  }))

  strange_speed <- sort(unique(c(strange_speed, short_time)))
  it_sp <- 0

  while (length(strange_speed) > 0 & it_sp < nb_iteration) {
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

    while (length(nextp) > 0 & it_strange_sp < nb_iteration_successive_strange) {
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
      dplyr::mutate(time_travelled = timestamp - c(dplyr::first(timestamp), timestamp[-n()]),
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
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(id_mmsi_point_initial != 1) %>%
      dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
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

    short_time <- na.omit(purrr::map_dbl(short_time, function(st) {
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
      dplyr::mutate(time_travelled = timestamp - c(dplyr::first(timestamp), timestamp[-n()]),
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

  if ("real_high_speed" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      dplyr::mutate(high_speed = real_high_speed) %>%
      dplyr::select(-real_high_speed)
  }

  ais_data <- ais_data %>%
    dplyr::select(dplyr::all_of(c(init_cols, "speed_kmh_corrected")))

  return(ais_data)
}
