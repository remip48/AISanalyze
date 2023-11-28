################
## function to compute function extract_AIS_presence() in an optimized way
## need :
## data : data frame with points where AIS must be extracted. Must contain timestamp, lon, lat
## ais_data : ais data containing timestamp, lon, lat, mmsi
## All the other parameter for the extract_AIS_presence() function
## filter_radius_m: NA to not filter, or distance in m to return only lines with these distances

## advice : run this function in a for or lapply loop to apply it per day. The function can return a big dataset with combination of mmsi find for each
## data point, so doing this decrease the final dataset returned. The function apply the process for each day in any case.
## dont use colnames idd

## return only data when dates are into ais data ?


# source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-1_extend_data_time.R")
# source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-2_extract_AIS.R")

#' Extract vessels data at desired times in a desired radius
#'
#' @param data Your data frame, with a column timestamp, lon and lat (numeric value of time, longitude, latitude)
#' @param ais_data AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity)
#' @param search_into_radius_m radius  in which vessels positions are returned
#' @param max_time_diff number of seconds before the data time, when boat positions are considered/extracted.
#' @param duplicate_time  if vessels positions must be extracted only for data times (TRUE) or every "t_gap" seconds up to max_time_diff before data times.
#' @param t_gap interval of time where vessels positions are extracted, from the data time to "max_time_diff" seconds before. You can also define the time interval where boat are considered with this value.
#' @param average_at number of seconds where times are averaged if accelerate = TRUE to decrease number of data time to extract. If average_at = 10, data times are averaged in the interval time-5:time+5. You can also define the time interval where boat are considered with this value.
#' @param accelerate TRUE or FALSE: if data times must be averaged within "average_at" seconds, to equlize times where vessels positions are extracted and decreased strongly computation time.
#' @param time_stop
#' @param mmsi_time_to_order
#' @param QUIET
#' @param correct_speed
#' @param quantile_station
#' @param threshold_distance_station
#' @param quantile_high_speed
#' @param threshold_speed_to_correct
#' @param threshold_high_speed
#' @param filter_station
#' @param interpolate_station
#' @param filter_high_speed
#' @param interpolate_high_speed
#' @param spatial_limit
#' @param on_Land_analysis
#' @param land_sf_polygon
#' @param return_all
#'
#' @return to add
#' @export
#'
#' @examples # to add
AIS_all_to_extract <- function(data,
                               ais_data,
                               # parallelize = T,
                               # core_to_use = NA,
                               parallelize = F,
                               nb_cores = 4,
                               outfile = "log.txt",
                               run_AIStravel = T,
                               save_AIStravel = F,
                               file_AIStravel = "AIStravel.rds",
                               run_AISinterpolate_at = T,
                               save_AISinterlate_at = F,
                               file_AISinterlate_at = "AISinterlate_at.rds",
                               run_AISextract_perHour = T,
                               save_AISextract_perHour = F,
                               file_AISextract_perHour = "AISextract.rds",
                               return_merged_all_extracted = T,
                               time_stop = Inf,
                               radius = Inf,
                               mmsi_time_to_order = T,
                               search_into_radius_m = 20000,
                               max_time_diff = 30 * 60,
                               duplicate_time = F,
                               t_gap = 15,
                               average_at = 10,
                               average_mmsi_at = 0,
                               # onLand = T,
                               accelerate = F,
                               QUIET = F,
                               correct_speed = T,
                               quantile_station = 0.95,
                               threshold_distance_station = 0,
                               quantile_high_speed = 0.90,
                               threshold_speed_to_correct = 100,
                               threshold_high_speed = 110,
                               filter_station = T,
                               interpolate_station = F,
                               filter_high_speed = T,
                               interpolate_high_speed = F,
                               spatial_limit = NA,
                               on_Land_analysis = F,
                               land_sf_polygon = NA,
                               overwrite = F
) {

  ais_data <- ais_data %>%
    dplyr::mutate(mmsi = ifelse(is.na(mmsi) | is.nan(mmsi) | is.null(mmsi), 0.1, mmsi))

  data <- data %>%
    dplyr::mutate(date = as.character(lubridate::date(lubridate::as_datetime(timestamp))))

  dates_ais <- as.character(unique(lubridate::date(lubridate::as_datetime(ais_data$timestamp))))
  dates_ais <- dates_ais[dates_ais %in% unique(data$date)]

  eff_d <- map_dfr(dates_ais, function(d) {
    eff_temp <- data %>%
      dplyr::filter(date == d) %>%
      dplyr::arrange(timestamp)

    ## if duplicate time, duplicate the time until max_time_diff secondes before each point, by interval times of t_gap.
    ## if accelerate, do this but decrease the number of times to extract by assigning the times to the closest time step designed (each one separated by t_gap up to max_time_diff before)
    if (duplicate_time) {
      eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
      # } else if (accelerate) {
      #   eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap)
    } else {
      eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
    }
    ## so here : the real time of effort is the timestamp_ofEffort, while we extract AIS with max_time = 0 on timestamp = timestamp_AIS_to_extract
    eff_temp <- eff_temp %>%
      dplyr::mutate(timestamp_ofEffort = timestamp,
                    timestamp = timestamp_AIS_to_extract)

    return(eff_temp)
  })

  rm(data)

  eff_d <- eff_d %>%
    dplyr::arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(hour_AIS_to_extract = lubridate::hour(datetime_AIS_to_extract),
                  dayhour = paste(lubridate::date(datetime_AIS_to_extract), hour_AIS_to_extract))

  if (!(all(c("X", "Y") %in% colnames(eff_d)))) {
    if (!("sf" %in% class(eff_d))) {
      eff_d <- eff_d %>%
        dplyr::mutate(tlon = lon,
                      tlat = lat) %>%
        st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }
    if (st_crs(eff_d)$input != "EPSG:3035") {
      eff_d <- eff_d %>%
        st_transform(crs = 3035)
    }

    coords_eff <- eff_d %>%
      st_coordinates() %>%
      as.data.frame()

    eff_d <- eff_d %>%
      st_drop_geometry() %>%
      dplyr::mutate(X = coords_eff[,1],
                    Y = coords_eff[,2])

    rm(coords_eff)
  }


  if (run_AIStravel) {
    cat("Estimating speed, distance and time travelled by MMSI\n")

    ais_data <- AIStravel(ais_data[ais_data$timestamp >= (min(eff_d$timestamp_AIS_to_extract, na.rm = T) - (t_gap + max_time_diff + average_at + time_stop)) &
                                     ais_data$timestamp <= (max(eff_d$timestamp_AIS_to_extract, na.rm = T) + t_gap + average_at + time_stop), ],
                          time_stop = time_stop,
                          mmsi_time_to_order = mmsi_time_to_order,
                          return_sf = F,
                          return_3035_coords = T)

    ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

    if (save_AIStravel) {
      saveRDS(ais_data, file = file_AIStravel)
    }
  }

  if (run_AISinterpolate_at) {
    cat("Interpolating MMSI positions for data times\n")

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

    ais_data <- AISinterpolate_at(ais_data = ais_data,
                                  mmsi_time_to_order = F,
                                  QUIET = QUIET,
                                  file_AISinterlate_at = file_AISinterlate_at,
                                  overwrite = overwrite,
                                  average_mmsi_at = average_mmsi_at,
                                  data_to_interpolate = (eff_d %>%
                                                           dplyr::select(timestamp, lon, lat, X, Y) %>%
                                                           distinct()),
                                  radius = radius,
                                  quantile_station = quantile_station,
                                  threshold_distance_station = threshold_distance_station,
                                  quantile_high_speed = quantile_high_speed,
                                  threshold_speed_to_correct = threshold_speed_to_correct,
                                  threshold_high_speed = threshold_high_speed,
                                  filter_station = filter_station,
                                  interpolate_station = interpolate_station,
                                  filter_high_speed = filter_high_speed,
                                  interpolate_high_speed = interpolate_high_speed,
                                  time_stop = time_stop,
                                  spatial_limit = spatial_limit,
                                  on_Land_analysis = on_Land_analysis,
                                  land_sf_polygon = land_sf_polygon,
                                  return_all = F,
                                  parallelize = parallelize,
                                  nb_cores = nb_cores,
                                  outfile = outfile)

    if (save_AISinterlate_at & (!file.exists(file_AISinterlate_at) | overwrite)) {
      saveRDS(ais_data, file = file_AISinterlate_at)
    }
  }

  # d_max <- (search_into_radius_m*1.5 + 2000) / (8*1e3)
  ## for 1 deg of difference, maximum distance of 111 km on the planet, minimum distance of 9.7

  if (run_AISextract_perHour) {
    ais_data_ref <- ais_data

    ais_data <- ais_data[ais_data$timestamp >= (min(eff_d$timestamp_AIS_to_extract, na.rm = T) - (t_gap + average_at +
                                                                                                    average_mmsi_at/2)) &
                           ais_data$timestamp <= (max(eff_d$timestamp_AIS_to_extract, na.rm = T) + t_gap + average_at +
                                                    average_mmsi_at/2) &
                           ais_data$X >= (min(eff_d$X) - search_into_radius_m) & ais_data$X <= (max(eff_d$X) + search_into_radius_m) &
                           ais_data$Y >= (min(eff_d$Y) - search_into_radius_m) & ais_data$Y <= (max(eff_d$Y) + search_into_radius_m), ]

    colnam <- colnames(ais_data)
    if (any(colnam[!(colnam %in% c("timestamp", "lon", "lat", "mmsi"))] %in% c(colnames(eff_d)))) {
      colnames(ais_data)[colnam %in% c(colnames(eff_d)) & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))] <- paste0("ais_", colnam[colnam %in% c(colnames(eff_d)) & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))])
      cat("\n", paste0("'", paste(colnam[colnam %in% c(colnames(eff_d)[!(colnames(eff_d) %in% c("X", "Y"))]) & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))], collapse = ", "), "'"), "columns in AIS data renamed as", paste0("'", paste(colnames(ais_data)[colnam %in% c(colnames(eff_d)[!(colnames(eff_d) %in% c("X", "Y"))]) & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))], collapse = ", "), "'"), "\n")
    }
    rm(colnam)

    if (any(colnames(eff_d) == "mmsi")) {
      colnames(eff_d)[colnames(eff_d) == "mmsi"] <- "initial_mmsi"
      cat("\nmmsi column in dataframe renamed as 'initial_mmsi'")
    }

    tot <- unique(eff_d$dayhour)

    cat("Extracting MMSI positions for data lon/lat\n")

    if (!parallelize) {
      if(!QUIET) {
        pb <- txtProgressBar(min = 0, max = length(tot), style = 3)
      }

      daily_ais <- map_dfr(tot, function(h) {
        if (!QUIET) {
          setTxtProgressBar(pb, match(h, tot))
        }

        eff_h <- eff_d %>%
          dplyr::filter(dayhour == h)

        if (overwrite |
            !(file.exists(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) |
            return_merged_all_extracted) {

          hourly_mmsi <- ais_data[ais_data$timestamp > (min(eff_h$timestamp, na.rm = T) - t_gap - average_at - average_mmsi_at/2) &
                                    ais_data$timestamp < (max(eff_h$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2) &
                                    ais_data$ais_X >= (min(eff_h$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_h$X) + search_into_radius_m) &
                                    ais_data$ais_Y >= (min(eff_h$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_h$Y) + search_into_radius_m), ] %>%
            dplyr::rename(ais_lon = lon,
                          ais_lat = lat)

          ais_on_effort <- data_extract_ais(data = eff_h,
                                            data_mmsi = hourly_mmsi,
                                            search_into_radius_m = search_into_radius_m,
                                            max_time_diff = 0,
                                            duplicate_time = F,
                                            t_gap = t_gap,
                                            average_at = 0,
                                            accelerate = F
          )

          # ais_on_effort <- ais_on_effort %>%
          #   dplyr::mutate(inside_daily_AIS_time_range = ifelse(timestamp >= (min(ais_data_ref$timestamp, na.rm = T) - t_gap - average_at) & timestamp <= (max(ais_data_ref$timestamp, na.rm = T) + t_gap + average_at),
          #                                                      T, F))

          ais_on_effort <- ais_on_effort %>%
            dplyr::mutate(timestamp = timestamp_ofEffort,
                          diffTime_AIS_effort = ais_timestamp - timestamp,
                          diffTime_AIS_extraction_effort = timestamp_AIS_to_extract - timestamp) %>%
            dplyr::select(!c("timestamp_ofEffort", "dayhour"))

          if (save_AISextract_perHour) {
            saveRDS(ais_on_effort,
                    file = paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
          }
          rm(eff_h)
          rm(hourly_mmsi)

          if (return_merged_all_extracted) {
            return(ais_on_effort)
          } else {
            return(NULL)
          }

        } else {

          if (return_merged_all_extracted &
              file.exists(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) {
            ais_on_effort <- readRDS(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
            return(ais_on_effort)
          } else {
            return(NULL)
          }
        }

      })

    }
    else {
      cl <- makeCluster(nb_cores, outfile = outfile)
      registerDoParallel(cl)

      daily_ais <- foreach(h = tot,
                           .packages = c("dplyr","tidyverse", "lubridate", "AISanalyze", "purrr", "sf", "stringr")
      ) %dopar% {
        if (!QUIET) {
          # setTxtProgressBar(pb, match(h, tot))
          cat(match(h, tot), "/", length(tot), "\n")
        }

        eff_h <- eff_d %>%
          dplyr::filter(dayhour == h)

        if (overwrite |
            !(file.exists(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) |
            return_merged_all_extracted) {

          hourly_mmsi <- ais_data[ais_data$timestamp > (min(eff_h$timestamp, na.rm = T) - t_gap - average_at - average_mmsi_at/2) &
                                    ais_data$timestamp < (max(eff_h$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2) &
                                    ais_data$ais_X >= (min(eff_h$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_h$X) + search_into_radius_m) &
                                    ais_data$ais_Y >= (min(eff_h$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_h$Y) + search_into_radius_m), ] %>%
          dplyr::rename(ais_lon = lon,
                        ais_lat = lat)

          ais_on_effort <- data_extract_ais(data = eff_h,
                                            data_mmsi = hourly_mmsi,
                                            search_into_radius_m = search_into_radius_m,
                                            max_time_diff = 0,
                                            duplicate_time = F,
                                            t_gap = t_gap,
                                            average_at = 0,
                                            accelerate = F
          )

          # ais_on_effort <- ais_on_effort %>%
          #   dplyr::mutate(inside_daily_AIS_time_range = ifelse(timestamp >= (min(ais_data_ref$timestamp, na.rm = T) - t_gap - average_at - average_mmsi_at/2) & timestamp <= (max(ais_data_ref$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2),
          #                                                      T, F))

          ais_on_effort <- ais_on_effort %>%
            dplyr::mutate(timestamp = timestamp_ofEffort,
                          diffTime_AIS_effort = ais_timestamp - timestamp,
                          diffTime_AIS_extraction_effort = timestamp_AIS_to_extract - timestamp) %>%
            dplyr::select(!c("timestamp_ofEffort", "dayhour"))

          if (save_AISextract_perHour) {
            saveRDS(ais_on_effort,
                    file = paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
          }
          rm(eff_h)
          rm(hourly_mmsi)
          if (return_merged_all_extracted) {
            return(ais_on_effort)
          } else {
            return(NULL)
          }

        } else {

          if (return_merged_all_extracted &
              file.exists(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) {
            ais_on_effort <- readRDS(paste0(str_remove_all(file_AISextract_perHour, ".rds"), "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
            return(ais_on_effort)
          } else {
            return(NULL)
          }
        }

      }

      stopCluster(cl)
      gc()

      daily_ais <- map_dfr(daily_ais, function(d) {return(d)})
    }

    rm(ais_data_ref)
  } else {
    daily_ais <- ais_data
  }

  # rm(d_max)
  rm(ais_data)
  rm(eff_d)
  rm(dates_ais)

  gc()

  return(daily_ais)

}
