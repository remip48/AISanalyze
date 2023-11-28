## to interpolate AIS data for each mmsi
## parameter :
## ais_data : data of AIS
## threshold_speed_fixed : speed, in km/h, where the AIS point is deleted and time and distance are estimated, for the next point, from the previous point
## threshold_speed_expr : function, with speed_kmh as parameter, filtering data with speed_kmh values higher than the function output. Example :
## threshold_speed_expr = function(speed_kmh) {median(speed_kmh, na.rm = T)}
## spatial_limit : sf object, of polygon, delimiting the area where to keep the interpolated AIS data
## threshold_high_speed : threshold in km/h where, when at least 90% of the AIS of a mmsi have speed superior, are removed (possible plane, or other ? but not boat)
## time_stop : time interval, in second, where interpolation is not performed anymore, due to uncertainties of AIS interpolation inside or meaning a pause of the ship track

## need : lon, lat, mmsi, timestamp, distance_travelled, time_travelled, speed_kmh
# data_to_interpolate contains lon, lat, timestamp

#' Interpolate vessel positions at the time desired
#'
#' @param ais_data AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity). Must be an output of AIStravel and contain distance_travelled, time_travelled, speed_kmh columns.
#' @param data_to_interpolate data times where vessel positions must be interpolate. Contain timestamp, lon and latitude columns
#' @param mmsi_time_to_order if mmsi & time column must be ordered, if FALSE, is already ordered by MMSI and then by timestamp (arrange(mmsi, timestamp)).
#' @param radius radius around data where interpolations are performed. Decrease the radius to decrease computation time.
#' @param QUIET to print steps along the process or not.
#' @param correct_speed if points with high and unrealistic speed must be removed and speed at this location corrected from previous and next point.
#' @param quantile_station if a mmsi have a percentage of data higher than this quantile, with distance_travelled < threshold_distance_station, this mmsi is designed as station (station = TRUE)
#' @param threshold_distance_station define threshold of distance, in meters, identifying stations among mmsi.
#' @param quantile_high_speed if a mmsi have a percentage of data higher than this quantile, with speed_kmh > threshold_high_speed, this mmsi is designed as high_speed (possible aircraft) (high_speed = TRUE)
#' @param threshold_speed_to_correct define unrealistic speed for correct_speed argument.
#' @param threshold_high_speed define threshold of speed, in km/h, identifying high_speed among mmsi.
#' @param filter_station if stations must be filtered.
#' @param filter_high_speed if high_speed mmsi must be filtered.
#' @param interpolate_station f stations must be interpolated.
#' @param interpolate_high_speed if high_speed mmsi must be interpolated.
#' @param time_stop time defining the maximum time interval where a vessel position is interpolated.
#' @param spatial_limit sf polygon defining the spatial limit of interpolation, otherwise let NA.
#' @param on_Land_analysis if a spatial analysis must be conducted to estimate time and distance of vessels and on interpolation on land (to determine AIS and interpolation error). Make computation longer.
#' @param land_sf_polygon if on_Land_analysis = TRUE, sf polygon containing the land.
#' @param return_all if all ais data must be returned, with interpolated positions, or only positions of the vessels at the time desired (smaller dataset).
#'
#' @return to add
#' @export
#'
#' @examples # to add
AISinterpolate_at <- function(ais_data,
                              data_to_interpolate,
                              overwrite,
                              file_AISinterlate_at,
                              mmsi_time_to_order = T,
                              average_mmsi_at = 0,
                              parallelize = F,
                              nb_cores = 4,
                              outfile = "log.txt",
                              QUIET = F,
                              radius = Inf, # km, or NA
                              correct_speed = T,
                              quantile_station = 0.95,
                              threshold_distance_station = 1,
                              quantile_high_speed = 0.90,
                              threshold_speed_to_correct = 90,
                              threshold_high_speed = 90,
                              filter_station = T,
                              filter_high_speed = T,
                              interpolate_station = F,
                              interpolate_high_speed = F,
                              time_stop = Inf,
                              spatial_limit = NA,
                              on_Land_analysis = T,
                              land_sf_polygon = NA,
                              return_all = F
){

  timestamp_to_interpolate <- na.omit(unique(data_to_interpolate$timestamp))

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

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(timestamp)
  }

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n())

  ais_data <- ais_data %>%
    dplyr::mutate(inside_temp = ifelse(timestamp >= min(data_to_interpolate$timestamp, na.rm = T) &
                                         timestamp <= max(data_to_interpolate$timestamp, na.rm = T), T, F),
                  before_temp = ifelse(timestamp < min(data_to_interpolate$timestamp, na.rm = T), T, F),
                  after_temp = ifelse(timestamp > max(data_to_interpolate$timestamp, na.rm = T), T, F))

  ais_data <- ais_data %>%
    group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) <= threshold_distance_station, T, F),
                  high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) >= threshold_high_speed, T, F),
                  any_NA_speed_kmh = ifelse(any(is.na(speed_kmh)), T, F),
                  n_point_mmsi_initial_data = n(),
                  id_mmsi_point_initial = 1:n()) %>%
    ungroup()

  if (return_all) {
    ais_data_ref <- ais_data %>%
      dplyr::mutate(speed_kmh_corrected = F) %>%
      dplyr::select(!c("inside_temp", "before_temp", "after_temp"))
  }

  if (radius != Inf) {
    ais_data <- ais_data[ais_data$X >= (min(data_to_interpolate$X, na.rm = T) - radius) & ais_data$X <= (max(data_to_interpolate$X, na.rm = T) + radius) &
                           ais_data$Y >= (min(data_to_interpolate$Y, na.rm = T) - radius) & ais_data$Y <= (max(data_to_interpolate$Y, na.rm = T) + radius), ]
  }
  if (time_stop != Inf) {
    ais_data <- ais_data[ais_data$timestamp >= (min(timestamp_to_interpolate) - (time_stop + average_mmsi_at)) &
                           ais_data$timestamp <= (max(timestamp_to_interpolate) + (time_stop + average_mmsi_at)), ]
  }

  gc()

  if (filter_station) {
    lines <- which(ais_data$station)

    to_remove <- ais_data[lines,]

    if (return_all) {
      ais_data_ref <- ais_data_ref %>%
        filter(!(mmsi %in% unique(to_remove$mmsi)))
    }

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }

    cat(length(lines), "data removed for STATION, i.e.", length(unique(to_remove$mmsi)), "stations\n")
    rm(to_remove)
  }
  if (filter_high_speed) {
    lines <- which(ais_data$high_speed)

    to_remove <- ais_data[lines,]

    if (return_all) {
      ais_data_ref <- ais_data_ref %>%
        filter(!(mmsi %in% unique(to_remove$mmsi)))
    }

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }
    cat(length(lines), "data removed for HIGH SPEED, i.e.", length(unique(to_remove$mmsi)), "high speed engine\n")
    rm(to_remove)
  }

  gc()

  ais_data <- ais_data %>%
    group_by(mmsi) %>%
    dplyr::filter(id_mmsi_point_initial %in% ifelse(any(inside_temp),
                                                    first(id_mmsi_point_initial[inside_temp]) - 1,
                                                    ifelse(any(before_temp),
                                                           last(which(before_temp)),
                                                           1)):ifelse(any(inside_temp),
                                                                      last(id_mmsi_point_initial[inside_temp]) + 1,
                                                                      ifelse(any(after_temp),
                                                                             first(which(after_temp)),
                                                                             n()))) %>%
    ungroup() %>%
    dplyr::select(!c("inside_temp", "before_temp", "after_temp"))

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

  raverage_mmsi_at <- round(average_mmsi_at/2, 0)
  timestamp_averaged <- unique(do.call("c", map(timestamp_to_interpolate, function(t) {
    return((t-raverage_mmsi_at):(t+raverage_mmsi_at))
  })))

  ais_ok <- ais_data[ais_data$timestamp %in% timestamp_averaged, ]

  to_run <- na.omit(map_int(timestamp_to_interpolate, function(t) {
    done <- ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)]

    return(ifelse(all(ais_data$mmsi %in% done), NA, t))
  }))

  # d_max <- (radius*1.5 + 2000) / (8*1e3)

  if (any(!is.na(spatial_limit))) {
    if (st_crs(spatial_limit)$input != "EPSG:3035") {
      spatial_limit <- spatial_limit %>%
        st_transform(crs = 3035)
    }
  }
  if (any(!is.na(land_sf_polygon))) {
    if (st_crs(land_sf_polygon)$input != "EPSG:3035") {
      land_sf_polygon <- land_sf_polygon %>%
        st_transform(crs = 3035)
    }
  }

  if (!interpolate_station) {
    list_station <- ais_data$mmsi[ais_data$station]
  }
  if (!interpolate_high_speed) {
    list_high_speed <- ais_data$mmsi[ais_data$high_speed]
  }

  if (!parallelize) {
    all_to_run <- to_run
    hour_to_run <- unique(lubridate::hour(lubridate::as_datetime(all_to_run)))

    if (!QUIET) {
      pb <- txtProgressBar(min = 0, max = length(all_to_run), style = 3)
    }

    ais_data <- map_dfr(hour_to_run, function(hh) {

      if (overwrite |
          !(file.exists(paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds")))) {
        to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

        out <- map_dfr(to_run, function(t) {
          if (!QUIET) {
            setTxtProgressBar(pb, match(t, to_run))
          }

          if (radius != Inf) {
            data_coords <- data_to_interpolate[data_to_interpolate$timestamp == t, ]

            if (!(all(c("X", "Y") %in% colnames(data_coords)))) {
              if (!("sf" %in% class(data_coords))) {
                data_coords <- data_coords %>%
                  dplyr::select(c("lon", "lat")) %>%
                  distinct() %>%
                  st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
              }
              if (st_crs(data_coords)$input != "EPSG:3035") {
                data_coords <- data_coords %>%
                  st_transform(crs = 3035)
              }

              data_coords <- data_coords %>%
                st_coordinates() %>%
                as.data.frame()
            }
          }

          done <- unique(ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)])

          temp <- ais_data

          if (length(done) > 0) {
            temp <- temp[!(temp$mmsi %in% done), ]
          }
          if (time_stop != Inf) {
            temp <- temp[temp$timestamp >= (t - (time_stop + average_mmsi_at)) &
                           temp$timestamp <= (t + (time_stop + average_mmsi_at)), ]
          }
          if (radius != Inf) {
            temp <- temp[temp$X >= (min(data_coords$X, na.rm = T) - radius) & temp$X <= (max(data_coords$X, na.rm = T) + radius) &
                           temp$Y >= (min(data_coords$Y, na.rm = T) - radius) & temp$Y <= (max(data_coords$Y, na.rm = T) + radius), ]
            rm(data_coords)
          }

          temp$difftimestamp <- temp$timestamp - t

          rm(done)

          sup <- temp[temp$difftimestamp > 0, ] %>%
            group_by(mmsi) %>%
            slice_min(difftimestamp) %>%
            ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          inf <- temp[temp$difftimestamp < 0, ] %>%
            group_by(mmsi) %>%
            slice_max(difftimestamp) %>%
            ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          rm(temp)

          to_interp <- rbind(inf, sup)

          if (nrow(to_interp) > 0) {
            n_point <- table(to_interp$mmsi)
            m_to_interp <- names(n_point)[n_point == 2]

            out_ok <- to_interp[to_interp$mmsi %in% names(n_point)[n_point == 1], ]

            if (!interpolate_station) {
              m_to_interp <- m_to_interp[!(m_to_interp %in% list_station)]
            }
            if (!interpolate_high_speed) {
              m_to_interp <- m_to_interp[!(m_to_interp %in% list_high_speed)]
            }

            if (length(m_to_interp) > 0) {
              prec <- inf[inf$mmsi %in% m_to_interp, ]

              interp_ref <- sup[sup$mmsi %in% m_to_interp, ]

              rm(sup)
              rm(inf)

              interp <- interp_ref %>%
                dplyr::mutate(ttimestamp = prec$timestamp,
                              tmmsi = prec$mmsi,
                              tlon = prec$lon,
                              tlat = prec$lat)

              if (any(interp$tmmsi != interp$mmsi)) {
                cat("CHECK THE CODE FOR tmmsi == mmsi\n")
                interp <- interp %>%
                  filter(tmmsi == mmsi)
              }

              rm(prec)

              if (on_Land_analysis) {
                interp <- interp %>%
                  group_by(id_ais_data_initial) %>%
                  dplyr::reframe(time_travelled = c(0, t - ttimestamp, timestamp - t),
                                 timestamp = c(ttimestamp, t, timestamp),
                                 speed_kmh = unique(speed_kmh),
                                 interpolated = c(F, T, F),
                                 distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                                 lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
                                 lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
                  )

                interp_eez <- interp %>%
                  left_join(interp_ref %>%
                              dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat", "X", "Y")),
                            by = "id_ais_data_initial") %>%
                  dplyr::mutate(id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = ".")))
              } else {
                interp_eez <- interp %>%
                  dplyr::mutate(interpolated = T,
                                time_travelled = t - ttimestamp,
                                distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                                lon = tlon + (lon - tlon) * time_travelled / (timestamp - ttimestamp),
                                lat = tlat + (lat - tlat) * time_travelled / (timestamp - ttimestamp),
                                timestamp = t,
                                id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = "."))
                  ) %>%
                  dplyr::select(-c("tlon", "tlat", "tmmsi", "ttimestamp")) ## , "X", "Y"
              }

              rm(to_interp)
              rm(interp)
              rm(interp_ref)

              interp_eez <- interp_eez %>%
                dplyr::mutate(tlon = lon,
                              tlat = lat) %>%
                st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
                st_transform(crs = 3035)

              if (!is.na(spatial_limit)) {
                to_keep <- colnames(interp_eez)

                interp_eez_int <- interp_eez %>%
                  dplyr::filter(interpolated)

                interp_eez <- interp_eez_int[st_intersects(interp_eez_int, spatial_limit, sparse = F), ] %>%
                  st_drop_geometry() %>%
                  dplyr::select(all_of(to_keep)) %>%
                  rbind(interp_eez %>%
                          filter(!interpolated))

                rm(interp_eez_int)
                rm(to_keep)
              }

              if (on_Land_analysis)  {
                ######### to check on land,
                interp_eez <- interp_eez %>%
                  dplyr::mutate(on_Land = c(ifelse(st_intersects(., land_sf_polygon, sparse = F) == F, F, T)))

                interp_eez_Land <- interp_eez %>%
                  group_by(mmsi) %>%
                  dplyr::summarise(do_union = F) %>%
                  st_cast("LINESTRING") %>%
                  ungroup() %>%
                  dplyr::mutate(total_distance_interp = units::drop_units(st_length(.))) %>%
                  st_intersection(., land_sf_polygon) %>%
                  dplyr::mutate(distance_interp_on_land = units::drop_units(st_length(.))) %>%
                  st_drop_geometry()

                interp_eez <- interp_eez %>%
                  left_join(interp_eez_Land %>%
                              dplyr::select(distance_interp_on_land, total_distance_interp, id_ais_data_initial) %>%
                              group_by(id_ais_data_initial) %>%
                              dplyr::summarise(total_distance_interp = unique(total_distance_interp),
                                               distance_interp_on_land = sum(distance_interp_on_land)),
                            by = "id_ais_data_initial")

                interp_eez <- interp_eez %>%
                  dplyr::filter(interpolated)

                rm(interp_eez_Land)
              }

              coords_ais <- interp_eez %>%
                st_coordinates() %>%
                as.data.frame()

              interp_eez <- interp_eez %>%
                st_drop_geometry() %>%
                dplyr::mutate(X = coords_ais[,1],
                              Y = coords_ais[,2])

              out <- map_dfr(list(out_ok,
                                  interp_eez),
                             function(d) {return(d)}) %>%
                dplyr::select(-"difftimestamp")

              return(out)

            } else {
              return(out_ok %>%
                       dplyr::select(-"difftimestamp"))
            }
          } else {
            return(to_interp %>%
                     dplyr::select(-"difftimestamp"))
          }
        })

        saveRDS(out, paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds"))

        gc()
      } else {
        out <- readRDS(paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds"))
      }

      return(out)
    })
  } else {
    all_to_run <- to_run
    hour_to_run <- unique(lubridate::hour(lubridate::as_datetime(all_to_run)))

    cl <- makeCluster(nb_cores, outfile = outfile)
    registerDoParallel(cl)

    ais_data <- map_dfr(hour_to_run, function(hh) {

      if (overwrite |
          !(file.exists(paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds")))) {
        to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

        out <- foreach(t = to_run,
                       .packages = c("dplyr","tidyverse", "sf")
        ) %dopar% {
          if (!QUIET) {
            # setTxtProgressBar(pb, match(t, timestamp_to_interpolate))
            # print(t)
            cat(match(t, all_to_run), "/", length(all_to_run), "\n")

          }

          if (radius != Inf) {
            data_coords <- data_to_interpolate[data_to_interpolate$timestamp == t, ]

            if (!(all(c("X", "Y") %in% colnames(data_coords)))) {
              if (!("sf" %in% class(data_coords))) {
                data_coords <- data_coords %>%
                  dplyr::select(c("lon", "lat")) %>%
                  distinct() %>%
                  st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
              }
              if (st_crs(data_coords)$input != "EPSG:3035") {
                data_coords <- data_coords %>%
                  st_transform(crs = 3035)
              }

              data_coords <- data_coords %>%
                st_coordinates() %>%
                as.data.frame()
            }
          }

          done <- unique(ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)])

          temp <- ais_data

          if (length(done) > 0) {
            temp <- temp[!(temp$mmsi %in% done), ]
          }
          if (time_stop != Inf) {
            temp <- temp[temp$timestamp >= (t - (time_stop + average_mmsi_at)) &
                           temp$timestamp <= (t + (time_stop + average_mmsi_at)), ]
          }
          if (radius != Inf) {
            temp <- temp[temp$X >= (min(data_coords$X, na.rm = T) - radius) & temp$X <= (max(data_coords$X, na.rm = T) + radius) &
                           temp$Y >= (min(data_coords$Y, na.rm = T) - radius) & temp$Y <= (max(data_coords$Y, na.rm = T) + radius), ]
            rm(data_coords)
          }

          temp$difftimestamp <- temp$timestamp - t

          rm(done)

          sup <- temp[temp$difftimestamp > 0, ] %>%
            group_by(mmsi) %>%
            slice_min(difftimestamp) %>%
            ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          inf <- temp[temp$difftimestamp < 0, ] %>%
            group_by(mmsi) %>%
            slice_max(difftimestamp) %>%
            ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          rm(temp)

          to_interp <- rbind(inf, sup)

          if (nrow(to_interp) > 0) {
            n_point <- table(to_interp$mmsi)
            m_to_interp <- names(n_point)[n_point == 2]

            out_ok <- to_interp[to_interp$mmsi %in% names(n_point)[n_point == 1], ]

            if (!interpolate_station) {
              m_to_interp <- m_to_interp[!(m_to_interp %in% list_station)]
            }
            if (!interpolate_high_speed) {
              m_to_interp <- m_to_interp[!(m_to_interp %in% list_high_speed)]
            }

            if (length(m_to_interp) > 0) {
              prec <- inf[inf$mmsi %in% m_to_interp, ]

              interp_ref <- sup[sup$mmsi %in% m_to_interp, ]

              rm(sup)
              rm(inf)

              interp <- interp_ref %>%
                dplyr::mutate(ttimestamp = prec$timestamp,
                              tmmsi = prec$mmsi,
                              tlon = prec$lon,
                              tlat = prec$lat)

              if (any(interp$tmmsi != interp$mmsi)) {
                cat("CHECK THE CODE FOR tmmsi == mmsi\n")
                interp <- interp %>%
                  filter(tmmsi == mmsi)
              }

              rm(prec)

              if (on_Land_analysis) {
                interp <- interp %>%
                  group_by(id_ais_data_initial) %>%
                  dplyr::reframe(time_travelled = c(0, t - ttimestamp, timestamp - t),
                                 timestamp = c(ttimestamp, t, timestamp),
                                 speed_kmh = unique(speed_kmh),
                                 interpolated = c(F, T, F),
                                 distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                                 lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
                                 lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
                  )

                interp_eez <- interp %>%
                  left_join(interp_ref %>%
                              dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat", "X", "Y")),
                            by = "id_ais_data_initial") %>%
                  dplyr::mutate(id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = ".")))
              } else {
                interp_eez <- interp %>%
                  dplyr::mutate(interpolated = T,
                                time_travelled = t - ttimestamp,
                                distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                                lon = tlon + (lon - tlon) * time_travelled / (timestamp - ttimestamp),
                                lat = tlat + (lat - tlat) * time_travelled / (timestamp - ttimestamp),
                                timestamp = t,
                                id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = "."))
                  ) %>%
                  dplyr::select(-c("tlon", "tlat", "tmmsi", "ttimestamp")) ## , "X", "Y"
              }

              rm(to_interp)
              rm(interp)
              rm(interp_ref)

              interp_eez <- interp_eez %>%
                dplyr::mutate(tlon = lon,
                              tlat = lat) %>%
                st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
                st_transform(crs = 3035)

              if (!is.na(spatial_limit)) {
                to_keep <- colnames(interp_eez)

                interp_eez_int <- interp_eez %>%
                  dplyr::filter(interpolated)

                interp_eez <- interp_eez_int[st_intersects(interp_eez_int, spatial_limit, sparse = F), ] %>%
                  st_drop_geometry() %>%
                  dplyr::select(all_of(to_keep)) %>%
                  rbind(interp_eez %>%
                          filter(!interpolated))

                rm(interp_eez_int)
                rm(to_keep)
              }

              if (on_Land_analysis)  {
                ######### to check on land,
                interp_eez <- interp_eez %>%
                  dplyr::mutate(on_Land = c(ifelse(st_intersects(., land_sf_polygon, sparse = F) == F, F, T)))

                interp_eez_Land <- interp_eez %>%
                  group_by(mmsi) %>%
                  dplyr::summarise(do_union = F) %>%
                  st_cast("LINESTRING") %>%
                  ungroup() %>%
                  dplyr::mutate(total_distance_interp = units::drop_units(st_length(.))) %>%
                  st_intersection(., land_sf_polygon) %>%
                  dplyr::mutate(distance_interp_on_land = units::drop_units(st_length(.))) %>%
                  st_drop_geometry()

                interp_eez <- interp_eez %>%
                  left_join(interp_eez_Land %>%
                              dplyr::select(distance_interp_on_land, total_distance_interp, id_ais_data_initial) %>%
                              group_by(id_ais_data_initial) %>%
                              dplyr::summarise(total_distance_interp = unique(total_distance_interp),
                                               distance_interp_on_land = sum(distance_interp_on_land)),
                            by = "id_ais_data_initial")

                interp_eez <- interp_eez %>%
                  dplyr::filter(interpolated)

                rm(interp_eez_Land)
              }

              coords_ais <- interp_eez %>%
                st_coordinates() %>%
                as.data.frame()

              interp_eez <- interp_eez %>%
                st_drop_geometry() %>%
                dplyr::mutate(X = coords_ais[,1],
                              Y = coords_ais[,2])

              out <- map_dfr(list(out_ok,
                                  interp_eez),
                             function(d) {return(d)}) %>%
                dplyr::select(-"difftimestamp")

              return(out)

            } else {
              return(out_ok %>%
                       dplyr::select(-"difftimestamp"))
            }
          } else {
            return(to_interp %>%
                     dplyr::select(-"difftimestamp"))
          }
        }

        out <- map_dfr(out, function(d) {return(d)})

        saveRDS(out, paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds"))

        gc()
      } else {
        out <- readRDS(paste0(str_remove_all(file_AISinterlate_at, ".rds"), "_", hh, ".rds"))
      }

      return(out)
    })

    stopCluster(cl)
    gc()

  }

  # rm(cols)

  out <- map_dfr(list(ais_ok,
                      ais_data %>%
                        distinct()),
                 function(d) {return(d)})

  if (return_all) {
    out <- map_dfr(list(out,
                        ais_data_ref %>%
                          dplyr::filter(!(id_ais_data_initial %in% out$id_ais_data_initial))), function(x) {return(x)})
  }

  out <- out  %>%
    arrange(mmsi, timestamp)

  if (!("interpolated") %in% colnames(out)) {
    out <- out %>%
      mutate(interpolated = NA)
  }

  out <- out %>%
    dplyr::mutate(datetime = as.character(lubridate::as_datetime(timestamp)),
                  date = as.character(lubridate::date(datetime)),
                  interpolated = ifelse(is.na(interpolated), F, interpolated))

  if ("id_mmsi_point_initial_real" %in% colnames(out)) {
    out <- out %>%
      mutate(id_mmsi_point_initial = id_mmsi_point_initial_real) %>%
      dplyr::select(-id_mmsi_point_initial_real)
  }

  rm(ais_ok)
  rm(ais_data)
  # rm(ais_data_ref)

  return(out)

}
