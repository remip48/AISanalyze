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
#' @param radius radius around data where interpolations are performed. Decrease the radius to decrease computation time.
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
#' @examples to add
AISinterpolate_at <- function(ais_data,
                              data_to_interpolate,
                              radius = 20000, # km, or NA
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
                              time_stop = 5 * 60 * 60,
                              spatial_limit = NA,
                              on_Land_analysis = T,
                              land_sf_polygon = NA,
                              return_all = F
){

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

  ais_data_ref <- ais_data

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
        arrange(id_ais_data_initial) # %>%
      # mutate(id_ais_data_initial = 1:n())

      rm(to_correct)
      rm(coords_AIS)
      rm(strange_speed)
    }
  }

  timestamp_to_interpolate <- unique(data_to_interpolate$timestamp)

  ais_ok <- ais_data %>%
    dplyr::filter(timestamp %in% timestamp_to_interpolate)

  ais_data <- ais_data[!(ais_data$mmsi %in% ais_ok$mmsi), ]

  # cols <- colnames(ais_data)[!(colnames(ais_data) %in%
  #                              c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat", "id_ais_data_initial"))]

  pb <- txtProgressBar(min = 0, max = length(timestamp_to_interpolate), style = 3)

  d_max <- (radius*1.5 + 2000) / (8*1e3)

  ais_data <- map_dfr(timestamp_to_interpolate, function(t) {
    setTxtProgressBar(pb, match(t, timestamp_to_interpolate))
    # print(t)

    data_coords <- data_to_interpolate %>%
      dplyr::select(lon, lat) %>%
      distinct()

    temp <- ais_data %>%
      filter(timestamp >= (t - time_stop) &
               timestamp <= (t + time_stop)) %>%
      dplyr::mutate(difftimestamp = timestamp - t) %>%
      dplyr::filter(lon > (min(data_coords$lon) - d_max) & lon < (max(data_coords$lon) + d_max) &
                      lat > (min(data_coords$lat) - d_max) & lat < (max(data_coords$lat) + d_max))

    temp <- temp %>%
      mutate(tlon = lon, tlat = lat) %>%
      st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
      st_transform(crs = 3035)

    coords <- st_coordinates(temp)

    data_coords <- data_coords %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      st_transform(crs = 3035) %>%
      st_coordinates() %>%
      as.data.frame()

    temp <- temp %>%
      st_drop_geometry() %>%
      mutate(X_ais = coords[,1],
             Y_ais = coords[,2]) %>%
      filter(X_ais >= (min(data_coords$X, na.rm = T) - radius) & X_ais <= (max(data_coords$X, na.rm = T) + radius) &
               Y_ais >= (min(data_coords$Y, na.rm = T) - radius) & Y_ais <= (max(data_coords$Y, na.rm = T) + radius)) %>%
      dplyr::select(-c(X_ais, Y_ais))

    rm(data_coords)
    rm(coords)

    sup <- temp %>%
      filter(difftimestamp > 0) %>%
      group_by(mmsi) %>%
      slice_min(difftimestamp) %>%
      ungroup()

    inf <- temp %>%
      filter(difftimestamp < 0) %>%
      group_by(mmsi) %>%
      slice_max(difftimestamp) %>%
      ungroup()

    rm(temp)

    to_interp <- rbind(inf, sup)

    if (nrow(to_interp) > 0) {
      to_interp <- to_interp %>%
        group_by(mmsi) %>%
        dplyr::mutate(npoint = n(),
                      idd = 1:n()) %>%
        ungroup()

      rm(sup)
      rm(inf)

      out_ok <- to_interp %>%
        filter(npoint == 1) %>%
        dplyr::select(-c(npoint, difftimestamp, idd))

      to_interp <- to_interp %>%
        filter(npoint == 2) %>%
        arrange(mmsi, timestamp)

      if (!interpolate_station) {
        to_interp <- to_interp %>%
          filter(!station)
      }
      if (!interpolate_high_speed) {
        to_interp <- to_interp %>%
          filter(!high_speed)
      }

      if (nrow(to_interp) > 0) {
        prec <- to_interp %>%
          filter(idd == 1)

        interp_ref <- to_interp %>%
          filter(idd == 2)

        interp <- interp_ref %>%
          mutate(ttimestamp = prec$timestamp,
                 tmmsi = prec$mmsi,
                 tlon = prec$lon,
                 tlat = prec$lat)

        rm(prec)

        interp <- interp %>%
          group_by(id_ais_data_initial) %>%
          dplyr::reframe(timestamp = c(ttimestamp, t, timestamp),
                         speed_kmh = unique(speed_kmh),
                         interpolated = c(F, T, F),
                         time_travelled = c(0, t - first(timestamp), last(timestamp) - t),
                         distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                         lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
                         lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
          )

        interp_eez <- interp %>%
          left_join(interp_ref %>%
                      dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat")),
                    by = "id_ais_data_initial") %>%
          dplyr::mutate(id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = ".")))

        rm(to_interp)
        rm(interp)
        rm(interp_ref)

        if (!is.na(spatial_limit)) {
          to_keep <- colnames(interp_eez)

          interp_eez_int <- interp_eez %>%
            filter(interpolated) %>%
            dplyr::mutate(tlon = lon,
                          tlat = lat) %>%
            st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
            st_transform(crs = 3035)

          interp_eez <- interp_eez_int[st_intersects(interp_eez_int, spatial_limit %>%
                                                       st_transform(crs = 3035), sparse = F), ] %>%
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
            dplyr::mutate(tlon = lon,
                          tlat = lat) %>%
            st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
            st_transform(crs = 3035) %>%
            dplyr::mutate(on_Land = c(ifelse(st_intersects(., land_sf_polygon %>%
                                                             st_transform(crs = 3035), sparse = F) == F, F, T)))

          interp_eez_Land <- interp_eez %>%
            group_by(mmsi) %>%
            dplyr::summarise(do_union = F) %>%
            st_cast("LINESTRING") %>%
            ungroup() %>%
            dplyr::mutate(total_distance_interp = units::drop_units(st_length(.))) %>%
            st_intersection(., land_sf_polygon %>%
                              st_transform(crs = 3035)) %>%
            dplyr::mutate(distance_interp_on_land = units::drop_units(st_length(.))) %>%
            st_drop_geometry()

          interp_eez <- interp_eez %>%
            left_join(interp_eez_Land %>%
                        dplyr::select(distance_interp_on_land, total_distance_interp, id_ais_data_initial) %>%
                        group_by(id_ais_data_initial) %>%
                        dplyr::summarise(total_distance_interp = unique(total_distance_interp),
                                         distance_interp_on_land = sum(distance_interp_on_land)),
                      by = "id_ais_data_initial")

          rm(interp_eez_Land)
        }

        out <- map_dfr(list(out_ok,
                            interp_eez %>%
                              filter(interpolated) %>%
                              dplyr::select(-c(npoint, difftimestamp, idd #, ttimestamp, tmmsi, tlon, tlat
                              ))),
                       function(d) {return(d)})

        return(out)

      } else {
        return(out_ok)
      }
    } else {
      return(to_interp)
    }
  })

  # rm(cols)

  out <- map_dfr(list(ais_ok,
                      ais_data %>%
                        distinct()),
                 function(d) {return(d)}) %>%
    arrange(mmsi, timestamp)

  if (return_all) {
    out <- out %>%
      rbind(ais_data_ref %>%
              filter(!(id_ais_data_initial %in% out$id_ais_data_initial)))
  }

  if (!("interpolated") %in% colnames(out)) {
    out <- out %>%
      mutate(interpolated = NA)
  }

  out <- out %>%
    dplyr::mutate(datetime = lubridate::as_datetime(timestamp),
                  # date = as.character(lubridate::date(datetime)),
                  interpolated = ifelse(is.na(interpolated), F, interpolated))

  rm(ais_ok)
  rm(ais_data)
  rm(ais_data_ref)

  return(out)

}
