#' Function to compute AIStravel, AISinterpolate_at and AISextract functions at the same time in an optimized way.
#' Used to extract AIS data in the radius of an input data with location and time.
#'
#' advice : run this function in a for or lapply loop to apply it per day. The function can return a big dataset with combination of mmsi find for each
#' data point, so doing this decrease the final dataset returned. The function apply the process for each day in any case.
#' dont use columns with the name "idd"
#'
#' @param data Data of interest for AIS extraction. Must contain a column "timestamp", "lon" and "lat" (numeric values).
#' @param ais_data AIS data. Must contain a column timestamp, lon, lat and mmsi (numeric value). the mmsi column is the identifier for vessel, and values can be replaced by the IMO for example, but the name of the column must be mmsi.
#' @param search_into_radius_m radius in which vessels positions are extracted.
#' @param max_time_diff number of seconds before the timestamp of every data timestamp, where boat positions are considered/extracted.
#' @param duplicate_time  if vessels positions must be extracted only for data timestamp (TRUE), or every "t_gap" seconds, up to "max_time_diff" before the data timestamps.
#' @param t_gap interval of time into which vessels positions are extracted, from the data timestamp up to "max_time_diff" seconds before. This defines also the time interval where boat are considered for the extraction.
#' @param average_at if accelerate = TRUE, average the data timestamps to +- average_at to decrease the number of data timestamp to process. This defines also the time interval where boat are considered for the extraction.
#' @param accelerate TRUE or FALSE: if data timestamps must be averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#' @param time_stop number of seconds around the AIS reception considered for interpolation of vessel positions. Interval of time higher than "time_stop" between 2 AIS receptions are considered as a stop of the movement. Filter also AIS data around data timestamp +- time_stop to accelerate the process.
#' @param mmsi_time_to_order if MMSI and time are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution.
#' @param QUIET if iterations are printed, either in the console if parallelize = F, or in the file "outfile" if parallelize = T.
#' @param correct_speed if speeds of vessel need to be corrected, to remove GPS errors/delay and unrealistic speeds.
#' @param quantile_station Quantile of distance by mmsi used to assess if the mmsi is a station or not. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.
#' @param threshold_distance_station threshold of distance used to assess if the mmsi is a station.
#' @param quantile_high_speed Quantile of speed by mmsi used to assess if the mmsi is a aircraft or not. We used 0.975 to prevent misinterpretations from GPS errors.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_high_speed threshold of speed used to assess if the mmsi is an aircraft.
#' @param filter_station if the stations are filtered or not.
#' @param interpolate_station if the stations are interpolated or not.
#' @param filter_high_speed if the aircraft are filtered or not.
#' @param interpolate_high_speed if the aircraft are interpolated or not.
#' @param spatial_limit sf polygon object of the area where outside points must be filtered out of the output. Not tested and might lead to few errors.
#' @param on_Land_analysis sf polygon object of the countries to study the reliability of GPS positions and interpolations with an analysis of the paths travelled by mmsi on land. Not tested and might lead to few errors.
#' @param parallelize if the interpolation and extract must be parallelized (required performant computer with increasing AIS dataframe and data timestamp to process.)
#' @param nb_cores number of cores to used for the parallelize
#' @param outfile file for output if parallelize = T
#' @param run_AIStravel if the AIS data must be ran with AIStravel function firstly or not. If already processed with AIStravel, probably not.
#' @param save_AIStravel if results from AIStravel must be saved, if run_AIStravel = T.
#' @param file_AIStravel if save_AIStravel = T, file where AIS data are saved after AIStravel.
#' @param run_AISinterpolate_at if the AIS data must be ran with AISinterpolate_at function firstly or not. If already processed with AISinterpolate_at, probably not.
#' @param save_AISinterlate_at if results from AISinterpolate_at must be saved, if run_AISinterpolate_at = T.
#' @param file_AISinterlate_at if save_AISinterlate_at = T, file where AIS data are saved after AISinterpolate_at
#' @param run_AISextract_perHour if the AIS data must be ran with AISextract, extracting AIS data for data timestamps, up to "max_time_diff".
#' @param save_AISextract_perHour if results from AISextract must be saved, if run_AISextract_perHour = T.
#' @param file_AISextract_perHour if save_AISextract_perHour = T, file where AIS data are saved after AISextract
#' @param return_merged_all_extracted if all extracted AIS data, merged with the data frame of input, must be returned. Otherwise, return NULL. Usefull if results have been saved during the process and the output is not necessary (decrease memory used).
#' @param radius radius within which AIS data are extracted around the data of input.
#' @param average_mmsi_at number of seconds where positions of mmsi are averaged for extraction. Less useful than average_at which average the data timestamps to process.
#' @param overwrite if any file must be written (save_AIStravel, save_AISinterlate_at, save_AISextract_perHour), if file are overwritten. Otherwise load the files if files are already existing and if the file is needed in the function.
#' @param land_sf_polygon if on_Land_analysis, sf polygon object for countries.
#'
#' @return return the input data with AIS extracted merged. Each line of input data is duplicated in the output data, a duplicate for each "t_gap" to extract up to "max_time_diff", and a duplicate for each MMSI present in the "radius" at the timestamp of interest. If no AIS are present in the radius, the columns dedicated to AIS data are filled with NA, so that no input data and no timestamp to extract is lost.
#' The output dataframe contains the columns of the input data, the columns of the AIS data (with "ais_" as prefix if the same column is already present in the input data), and the following columns:
#' distance_effort_ais_m: distance (meters) between the data point and the MMSI for this line (filled with NA if no MMSI).
#' timestamp_AIS_to_extract: timestamp for the extraction of the AIS for this line (= data timestamp if average_at = 0 & average_mmsi_at = 0 & accelerate = F).
#' diffTime_AIS_extraction_effort: difference, in seconds, between the timestamp to extract (timestamp_AIS_to_extract) and the data timestamp.
#' datetime_AIS_to_extract: datetime (ymd_hms) of timestamp_AIS_to_extract
#' hour_AIS_to_extract: hour of timestamp_AIS_to_extract
#' time_travelled: number of seconds since the last reception of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessel since the last reception of an AIS signal.
#' id_ais_data_initial: identifier of the row line in the ais data, ordered, corrected and cleaned. Use for internal computation. For interpolated data, id_ais_data_initial is the same than the next real existing line.
#' station: if the MMSI is a station or not.
#' high_speed: if the MMSI is a high speed craft (used for aircraft) or not.
#' any_NA_speed_kmh: if any of the MMSI point has a value of speed of NA (so distance_travelled or time_travelled has a issue and the MMSI points must be checked). Should not occur.
#' n_point_mmsi_initial_data: number of point of the MMSI in the initial AIS data, removing firstly the inexisting longitude and latitude points.
#' id_mmsi_point_initial: identifier for the MMSI point in the ordered, corrected and cleaned AIS data.
#' speed_kmh_corrected: if the speed of this line has been corrected or not.
#' interpolated: if this AIS position is an interpolation or not.
#' diffTime_AIS_effort: difference, in seconds, between the AIS position and the data timestamp.
#'
#' @export
#'
#' @examples # to add
AIStravel_interpolate_extract <- function(data,
                               ais_data,
                               parallelize = F,
                               nb_cores = NA,
                               outfile = "log.txt",
                               run_AIStravel = T,
                               save_AIStravel = T,
                               file_AIStravel = "AIStravel.rds",
                               run_AISinterpolate_at = T,
                               save_AISinterlate_at = T,
                               file_AISinterlate_at = "AISinterlate_at.rds",
                               run_AISextract_perHour = T,
                               save_AISextract_perHour = T,
                               file_AISextract_perHour = "AISextract.rds",
                               return_merged_all_extracted = T,
                               time_stop = 5*60*60,
                               radius = 200000,
                               mmsi_time_to_order = T,
                               search_into_radius_m = 50000,
                               max_time_diff = 1 * 60 * 60,
                               duplicate_time = T,
                               t_gap = 2*60,
                               average_at = 30,
                               average_mmsi_at = 0,
                               accelerate = T,
                               QUIET = F,
                               correct_speed = T,
                               quantile_station = 0.975,
                               threshold_distance_station = 10,
                               quantile_high_speed = 0.97,
                               threshold_speed_to_correct = 100,
                               threshold_high_speed = 110,
                               filter_station = T,
                               interpolate_station = T,
                               filter_high_speed = T,
                               interpolate_high_speed = T,
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

  eff_d <- purrr::map_dfr(dates_ais, function(d) {
    eff_temp <- data %>%
      dplyr::filter(date == d) %>%
      dplyr::arrange(timestamp)

    ## if duplicate time, duplicate the time until max_time_diff secondes before each point, by interval times of t_gap.
    ## if accelerate, do this but decrease the number of times to extract by assigning the times to the closest time step designed (each one separated by t_gap up to max_time_diff before)
    if (duplicate_time) {
      eff_temp <- DATAextend_time(data = eff_temp, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
      # } else if (accelerate) {
      #   eff_temp <- DATAextend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap)
    } else {
      eff_temp <- DATAextend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
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

      daily_ais <- purrr::map_dfr(tot, function(h) {
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

          ais_on_effort <- AISextract(data = eff_h,
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

          ais_on_effort <- AISextract(data = eff_h,
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

      daily_ais <- purrr::map_dfr(daily_ais, function(d) {return(d)})
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
