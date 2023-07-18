## to interpolate AIS data for each mmsi
## parameter :
## ais_data : data of AIS
## threshold_speed_fixed : speed, in km/h, where the AIS point is deleted and time and distance are estimated, for the next point, from the previous point
## threshold_speed_expr : function, with speed_kmh as parameter, filtering data with speed_kmh values higher than the function output. Example :
## threshold_speed_expr = function(speed_kmh) {median(speed_kmh, na.rm = T)}
## t_gap : the interval time to interpolate AIS data
## spatial_limit : sf object, of polygon, delimiting the area where to keep the interpolated AIS data
## threshold_high_speed : threshold in km/h where, when at least 90% of the AIS of a mmsi have speed superior, are removed (possible plane, or other ? but not boat)
## time_stop : time interval, in second, where interpolation is not performed anymore, due to uncertainties of AIS interpolation inside or meaning a pause of the ship track

## need : lon, lat, mmsi, timestamp, distance_travelled, time_travelled, speed_kmh

#' Interpolate all vessels positions.
#'
#' @param ais_data AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity). Must be an output of AIStravel and contain distance_travelled, time_travelled, speed_kmh columns.
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
#' @param t_gap interval of time where vessels positions are extracted, from the data time to "max_time_diff" seconds before.
#' @param spatial_limit sf polygon defining the spatial limit of interpolation, otherwise let NA.
#' @param on_Land_analysis if a spatial analysis must be conducted to estimate time and distance of vessels and on interpolation on land (to determine AIS and interpolation error). Make computation longer.
#' @param land_sf_polygon if on_Land_analysis = TRUE, sf polygon containing the land.
#'
#' @return to add
#' @export
#'
#' @examples to add
AISinterpolate_all <- function(ais_data,
                               # parallelize=T,
                               # core_to_use=NA,
                               correct_speed = T,
                               quantile_station = 0.95,
                               threshold_distance_station = 1,
                               quantile_high_speed = 0.90,
                               threshold_speed_to_correct = 90,
                               # threshold_speed_expr = function(speed_kmh) {threshold_speed_fixed},
                               threshold_high_speed = 90,
                               filter_station = T,
                               filter_high_speed = T,
                               interpolate_station = F,
                               interpolate_high_speed = F,
                               time_stop = 5 * 60 * 60,
                               t_gap = 4,
                               spatial_limit = NA,
                               on_Land_analysis = T,
                               land_sf_polygon = NA){

  # load_packages <- function(p) {
  #   if (p %in% rownames(installed.packages())) {
  #     library(p, character.only = TRUE)
  #   } else {
  #     install.packages(p)
  #     library(p, character.only = TRUE)
  #   }
  # }
  # lapply(c("tidyverse", "dplyr", "sf", "lubridate"), load_packages)

  ais_data <- ais_data %>%
    group_by(mmsi) %>%
    mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) < threshold_distance_station, T, F),
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

  ais_data <- ais_data %>%
    arrange(mmsi, timestamp) %>%
    mutate(id_ais_data_initial = 1:n())

  if (correct_speed) {
    strange_speed <- ais_data[ais_data$speed_kmh > threshold_speed_to_correct & ais_data$time_travelled < time_stop & !ais_data$high_speed, ] %>%
      pull(id_ais_data_initial)

    if (length(strange_speed) > 0) {
      to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]

      to_correct <- to_correct %>%
        arrange(mmsi, timestamp) %>%
        mutate(tlon = lon,
               tlat = lat
        ) %>%
        st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
        st_transform(crs = 3035)

      coords_AIS <- to_correct %>%
        st_coordinates()

      to_correct <- to_correct %>%
        st_drop_geometry() %>%
        mutate(X = coords_AIS[,1],
               Y = coords_AIS[,2]) %>%
        group_by(mmsi) %>%
        mutate(distance_travelled = c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2)),
               time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
               speed_kmh = c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1]))
        ) %>%
        ungroup() %>%
        dplyr::select(-c("X", "Y"))

      to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ]

      ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
        rbind(to_correct) %>%
        arrange(id_ais_data_initial) %>%
        mutate(id_ais_data_initial = 1:n())

      rm(to_correct)
      rm(coords_AIS)
      rm(strange_speed)
    }
  }

  to_interp <- ais_data %>%
    group_by(mmsi) %>%
    mutate(idd = 1:n()) %>%
    filter(idd != 1) %>%
    ungroup() %>%
    dplyr::select(-idd)

  to_interp <- to_interp[to_interp$time_travelled > 1.5*t_gap & to_interp$time_travelled < time_stop, ]

  if (!interpolate_station) {
    to_interp <- to_interp %>%
      filter(!station)
  }
  if (!interpolate_high_speed) {
    to_interp <- to_interp %>%
      filter(!high_speed)
  }

  prec <- ais_data[ais_data$id_ais_data_initial %in% (to_interp$id_ais_data_initial - 1), ]

  interp <- to_interp %>%
    mutate(ttimestamp = prec$timestamp,
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
    filter(!(ais_data$id_ais_data_initial %in% unique(interp_eez$id_ais_data_initial))) %>%
    mutate(diffTime_interpolation = t_gap,
           interpolated = F) %>%
    rbind(interp_eez) %>%
    arrange(mmsi, timestamp) %>%
    group_by(mmsi) %>%
    mutate(n_point_mmsi_final_data = n(),
           id_mmsi_point_interp = 1:n(),
           datetime = lubridate::as_datetime(timestamp),
           date = as.character(date(datetime))) %>%
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
      mutate(sum_distance_mmsi_on_Land = sum(distance_travelled[on_Land == T], na.rm = T),
             total_distance_mmsi = sum(distance_travelled, na.rm = T),
             sum_time_mmsi_on_Land = sum(time_travelled[on_Land == T], na.rm = T),
             total_time_mmsi = sum(time_travelled, na.rm = T)) %>%
      ungroup() %>%
      group_by(mmsi, id_mmsi_point_initial) %>%
      mutate(sum_distance_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
             sum_time_interp_on_Land_for_initial_points = ifelse(any(interpolated), sum(time_travelled[on_Land == T & interpolated == T], na.rm = T), NA),
             sum_distance_interp_for_initial_points = ifelse(any(interpolated), sum(distance_travelled[interpolated == T], na.rm = T), NA),
             sum_time_interp_for_initial_points = ifelse(any(interpolated), sum(time_travelled[interpolated == T], na.rm = T), NA),
             perc_distance_interp_for_initial_points = dist_interp_idd_on_Land / dist_interp_idd,
             perc_time_interp_for_initial_points = time_interp_idd_on_Land / time_interp_idd) %>%
      ungroup()
  }

  return(ais_data)

}
