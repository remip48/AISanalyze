#' Run AIStravel, AIScorrect_speed, AISinterpolate_at and AISextract together in an optimized way.
#' Used to extract AIS data in the radius of an input data with location and time.
#'
#' Useful to run the complete process in a single function. A check of intermediate results is recommended, to adapt parameters if needed. Save the intermediate results during the processes. Results from the function MMSI_infos can be merge to this output to add information on vessel type and length.
#' Advice : if AIS data and data to extract are large, and if the application allows it, run this function in a loop for each day to save memory for computation.
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param search_into_radius_m radius (kilometer) where the MMSIs are extracted and returned.
#' @param load_existing_files if TRUE, load the existing files (saved during previous runs) of AIStravel, AISinterpolate_at and AISextract when running the functions.
#' @param run_AIStravel if the AIS data must be ran with AIStravel function firstly or not. If already processed with AIStravel, probably not.
#' @param save_AIStravel if results from AIStravel must be saved (only if run_AIStravel = T).
#' @param file_AIStravel if save_AIStravel = TRUE, is the file name where AIStravel output is saved. Must not contain file format: the files are written as .rds.
#' @param overwrite if TRUE, the saved files (see save_AIStravel, save_AISinterlate_at, save_AISextract_perHour) overwrite existing files. Otherwise load the existing files if these are existing and needed in the function.
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param run_AISinterpolate_at if the AIS data must be ran with AISinterpolate_at function firstly or not. If already processed with AISinterpolate_at, probably not.
#' @param save_AISinterlate_at if TRUE, save the results for each iteration of hour of AIS data (only if run_AISinterpolate_at = T)
#' @param file_AISinterlate_at if save_AISinterlate_at = TRUE, is the file name where hourly interpolated AIS data are saved. Must not contain file format: the files are written as .rds.
#' @param correct_speed if TRUE, GPS errors and GPS delays are identified and removed from AIS data. Vessel speeds, distance and time travelled are corrected. Usually necessary.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions.
#' @param duplicate_time if TRUE, extend (and duplicate) the data to past timestamps, to investigate the past presence of vessels at the data locations: extend the data timestamps up to "max_time_diff" number of seconds before the timestamps, by steps of "t_gap" number of seconds.
#' @param max_time_diff if duplicate_time = TRUE, extend (and duplicate) the data to past timestamps, to investigate the past presence of vessels at the data locations: extend the data timestamps up to "max_time_diff" number of seconds before the timestamps, by steps of "t_gap" number of seconds.
#' @param t_gap see "max_time_diff". Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction (otherwise other AIS data are filtered out).
#' @param accelerate if TRUE, data timestamps are averaged at "average_at" seconds to decrease the number of data timestamp to process and  strongly decrease the computation time.
#' @param average_at if accelerate = TRUE, the data timestamps are approximated to within to "average_at" number of seconds. This, to decrease the number of data timestamps to process. Necessary for large data timestamps to extract. Is also used as the number of seconds before and after the data timestamps where vessels are considered for extraction, in addition to "t_gap" parameter (otherwise other AIS data are filtered out).
#' @param filter_station if TRUE, filter the stations out.
#' @param filter_high_speed if TRUE, filter the aircraft out.
#' @param interpolate_station if FALSE, do not interpolate the positions of the stations.
#' @param interpolate_high_speed if FALSE, do not interpolate the positions of the aircrafts.
#' @param radius radius (kilometers) around data where AIS data are considered for interpolation of the positions. Must be large enough to collect the AIS data necessary for a linear interpolation at the time of the data. Is used also to filter the AIS data too far from the data of interest and slowing the processes (we used 200 km as default value of radius).
#' @param quantile_station Quantile (0 to 1) of distance, by mmsi, which is compared to threshold_distance_station to assess if the MMSI is a station or not: if below threshold_distance_station, MMSI is considered as stationary and is a station. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.
#' @param threshold_distance_station Threshold of distance (meters) used to assess if the MMSI is a station.
#' @param quantile_high_speed Quantile (0 to 1) of speed, by mmsi, which is compared to threshold_high_speed to assess if the MMSI is a aircraft or not: if above threshold_high_speed, MMSI is considered as a station. We used 0.97 to prevent misinterpretations from GPS errors.
#' @param threshold_high_speed Threshold of speed (km/h) used to assess if the MMSI is an aircraft.
#' @param run_AISextract_perHour if the AIS data must be extract for the locations & times of the data (ran with AISextract), up to "max_time_diff" number of seconds before the data timestamps by steps of "t_gap".
#' @param save_AISextract_perHour if results from AISextract must be saved, (only if run_AISextract_perHour = T).
#' @param file_AISextract_perHour if save_AISextract_perHour = TRUE, is the file name where hourly extracted AIS data along data of interest are saved. Must not contain file format: the files are written as .rds.
#' @param return_merged_all_extracted if TRUE, return the output (from AISextract, AISinterpolate_at, or AIStravel by order of process). Otherwise, return NULL. Usefull if the results are saved during the process and the output is not necessary (set it as FALSE and decrease the memory used).
#' @param parallelize if TRUE, parallelize with "doParallel" package the processes (required powerful computer if large AIS dataset and data timestamps to process.)
#' @param nb_cores number of cores to used with doParallel.
#' @param outfile file to print the logs if parallelize = T.
#' @param QUIET if TRUE, print the iterations: either in the console if parallelize = F, or in the file "outfile" if parallelize = T.
#' @param load_existing_files
#'
#' @return return the input data with the AIS extracted merged in the dataframe: each line of input data is duplicated by timestamp to extract (every "t_gap" number of seconds up to "max_time_diff" number of seconds). All these lines are duplicated for each MMSI present in the area at the moment of the extraction. If no AIS are present in the radius at this moment, the columns dedicated to AIS data are filled with NA, so that no input data and no timestamp to extract is lost.
#' The output dataframe contains the columns of the input data, the columns of the AIS data (with "ais_" as prefix if the same column is already present in the input data), and the following columns:
#' distance_effort_ais_m: distance (meters) between the data location and the MMSI at this time (filled with NA if no MMSI).
#' timestamp_AIS_to_extract: timestamp for the extraction of the AIS (approximated with "average_at" number of seconds if accelerate = TRUE).
#' diffTime_AIS_extraction_effort: difference (in seconds) between the timestamp to extract (timestamp_AIS_to_extract) and the real data timestamp.
#' datetime_AIS_to_extract: datetime of timestamp_AIS_to_extract.
#' diffTime_AIS_effort: difference, in seconds, between the AIS data and the data timestamp: can be different from the difference between the timestamp of the extraction and the real data timestamp (diffTime_AIS_extraction_effort) due to the parameter "average_at" & "t_gap"
#' hour_AIS_to_extract: hour of timestamp_AIS_to_extract
#' time_travelled: number of seconds since the last reception or interpolation of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception or interpolation of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessels since the last reception or interpolation of an AIS signal.#' id_ais_data_initial: identifier of the row line in the ais data, ordered, corrected and cleaned. Use for internal computation. For interpolated data, id_ais_data_initial is the same than the next real existing line.
#' id_ais_data_initial: identifier of the row in the ordered, corrected and cleaned ais data. Used for internal computation. For interpolated positions, id_ais_data_initial is the same than the next real existing AIS data.
#' station: if TRUE, the MMSI has been identified as a station.
#' high_speed: if TRUE, the MMSI has been identified as an high speed craft (specially used for aircraft).
#' any_NA_speed_kmh: if TRUE, at least one of the speeds of this MMSI has a speed as NA (so distance_travelled or time_travelled has a issue and the AIS data must be checked). Should not occur.
#' n_point_mmsi_initial_data: number of point of the MMSI in the AIS data after have removed the points with inexisting longitude and latitude.
#' id_mmsi_point_initial: identifier for the MMSI point after ordering, correcting and cleaning.
#' speed_kmh_corrected: if TRUE, the speed of the line has been corrected.
#' interpolated: if TRUE, this MMSI position has been interpolated.
#'
#' @export
#'
#' @examples # to add
AIStravel_interpolate_extract <- function(data,
                                          ais_data,
                                          mmsi_time_to_order = T,
                                          search_into_radius_m = 50000,
                                          run_AIStravel = T,
                                          save_AIStravel = T,
                                          file_AIStravel = "AIStravel",
                                          load_existing_files = F,
                                          overwrite = F,
                                          time_stop = 5*60*60,
                                          run_AISinterpolate_at = T,
                                          save_AISinterlate_at = T,
                                          file_AISinterlate_at = "AISinterpolate_at",
                                          correct_speed = T,
                                          threshold_speed_to_correct = 100,
                                          threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 0], na.rm = T) +
                                                                                                           sd(speed_kmh[speed_kmh > 0 & speed_kmh < quantile(speed_kmh, 0.75, na.rm = T)])*2.5 + 15))},
                                          duplicate_time = T,
                                          max_time_diff = 1 * 60 * 60,
                                          t_gap = 2*60,
                                          accelerate = T,
                                          average_at = 30,
                                          filter_station = T,
                                          filter_high_speed = T,
                                          interpolate_station = T,
                                          interpolate_high_speed = T,
                                          radius = 200000,
                                          quantile_station = 0.975,
                                          threshold_distance_station = 10,
                                          quantile_high_speed = 0.97,
                                          threshold_high_speed = 110,
                                          run_AISextract_perHour = T,
                                          save_AISextract_perHour = T,
                                          file_AISextract_perHour = "AISextract",
                                          return_merged_all_extracted = T,
                                          parallelize = F,
                                          nb_cores = NA,
                                          outfile = "log.txt",
                                          # average_mmsi_at = 0,
                                          QUIET = F
                                          # spatial_limit = NA,
                                          # on_Land_analysis = F,
                                          # land_sf_polygon = NA,
) {

  # param spatial_limit sf polygon object of the area where outside points must be filtered out of the output. Not tested and might lead to few errors.
  # param on_Land_analysis sf polygon object of the countries to study the reliability of GPS positions and interpolations with an analysis of the paths travelled by mmsi on land. Not tested and might lead to few errors.
  # param land_sf_polygon if on_Land_analysis, sf polygon object for countries.
  average_mmsi_at <- 0
  # param average_mmsi_at number of seconds where positions of mmsi are averaged for extraction. Less useful than average_at which average the data timestamps to process.

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

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

  rm(dates_ais)
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

    if (load_existing_files & file.exists(file = paste0(file_AIStravel, ".rds")) & !overwrite) {
      ais_data <- readRDS(file = paste0(file_AIStravel, ".rds"))
    }
    else {
      ais_data <- AIStravel(ais_data = ais_data[ais_data$timestamp >= (min(eff_d$timestamp_AIS_to_extract, na.rm = T) - (t_gap + max_time_diff + average_at + time_stop)) &
                                                  ais_data$timestamp <= (max(eff_d$timestamp_AIS_to_extract, na.rm = T) + t_gap + average_at + time_stop), ],
                            time_stop = time_stop,
                            mmsi_time_to_order = mmsi_time_to_order,
                            return_sf = F,
                            return_3035_coords = T)

      ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

      if (save_AIStravel & (!file.exists(paste0(file_AIStravel, ".rds")) | overwrite)) {
        saveRDS(ais_data, file = paste0(file_AIStravel, ".rds"))
      }
    }

    cat("\n______________________________________________________________________\n\n")
  }

  if (run_AISinterpolate_at) {
    cat("Interpolating MMSI positions for data times\n")

    # if (any(!is.na(spatial_limit))) {
    #   if (st_crs(spatial_limit)$input != "EPSG:3035") {
    #     spatial_limit <- spatial_limit %>%
    #       st_transform(crs = 3035)
    #   }
    # }
    # if (any(!is.na(land_sf_polygon))) {
    #   if (st_crs(land_sf_polygon)$input != "EPSG:3035") {
    #     land_sf_polygon <- land_sf_polygon %>%
    #       st_transform(crs = 3035)
    #   }
    # }

    if (load_existing_files & !overwrite & file.exists(file = paste0(file_AISinterlate_at, ".rds"))) {
      ais_data <- readRDS(paste0(file_AISinterlate_at, ".rds"))
    }

    ais_data <- AISinterpolate_at(ais_data = ais_data,
                                  mmsi_time_to_order = F,
                                  QUIET = QUIET,
                                  load_existing_files = load_existing_files,
                                  file_AISinterlate_at = file_AISinterlate_at,
                                  overwrite = overwrite,
                                  # average_mmsi_at = average_mmsi_at,
                                  data = (eff_d %>%
                                            dplyr::select(timestamp, lon, lat, X, Y) %>%
                                            dplyr::distinct()),
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
                                  save_AISinterlate_at = save_AISinterlate_at,
                                  time_stop = time_stop,
                                  threshold_speed_to_correct_expr = threshold_speed_to_correct_expr,
                                  # spatial_limit = spatial_limit,
                                  # on_Land_analysis = on_Land_analysis,
                                  # land_sf_polygon = land_sf_polygon,
                                  # return_all = F,
                                  parallelize = parallelize,
                                  nb_cores = nb_cores,
                                  outfile = outfile)

    if (save_AISinterlate_at & (!file.exists(paste0(file_AISinterlate_at, ".rds")) | overwrite)) {
      saveRDS(ais_data, file = paste0(file_AISinterlate_at, ".rds"))
    }
    cat("\n\n______________________________________________________________________\n\n")

  }

  # d_max <- (search_into_radius_m*1.5 + 2000) / (8*1e3)
  ## for 1 deg of difference, maximum distance of 111 km on the planet, minimum distance of 9.7

  if (run_AISextract_perHour) {
    # ais_data_ref <- ais_data

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
            !(file.exists(paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds")))) {

          hourly_mmsi <- ais_data[ais_data$timestamp > (min(eff_h$timestamp, na.rm = T) - t_gap - average_at - average_mmsi_at/2) &
                                    ais_data$timestamp < (max(eff_h$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2) &
                                    ais_data$ais_X >= (min(eff_h$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_h$X) + search_into_radius_m) &
                                    ais_data$ais_Y >= (min(eff_h$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_h$Y) + search_into_radius_m), ] %>%
            dplyr::rename(ais_lon = lon,
                          ais_lat = lat)

          ais_on_effort <- AISextract(data = eff_h,
                                      ais_data = hourly_mmsi,
                                      search_into_radius_m = search_into_radius_m,
                                      max_time_diff = 0,
                                      duplicate_time = F,
                                      t_gap = t_gap,
                                      average_at = average_at,
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
                    file = paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
            cat("\nFILE", paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"), "SAVED\n")
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
              file.exists(paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) {

            cat("\nLOAD FILE", paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"), "\n")

            ais_on_effort <- readRDS(paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
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
                           .export = ls(),
                           .packages = c("dplyr","tidyverse", "lubridate", "AISanalyze", "purrr", "sf", "stringr")
      ) %dopar% {
        if (!QUIET) {
          # setTxtProgressBar(pb, match(h, tot))
          cat(match(h, tot), "/", length(tot), "\n")
        }

        eff_h <- eff_d %>%
          dplyr::filter(dayhour == h)

        if (overwrite |
            !(file.exists(paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds")))) {

          hourly_mmsi <- ais_data[ais_data$timestamp > (min(eff_h$timestamp, na.rm = T) - t_gap - average_at - average_mmsi_at/2) &
                                    ais_data$timestamp < (max(eff_h$timestamp, na.rm = T) + t_gap + average_at + average_mmsi_at/2) &
                                    ais_data$ais_X >= (min(eff_h$X) - search_into_radius_m) & ais_data$ais_X <= (max(eff_h$X) + search_into_radius_m) &
                                    ais_data$ais_Y >= (min(eff_h$Y) - search_into_radius_m) & ais_data$ais_Y <= (max(eff_h$Y) + search_into_radius_m), ] %>%
          dplyr::rename(ais_lon = lon,
                        ais_lat = lat)

          ais_on_effort <- AISextract(data = eff_h,
                                      ais_data = hourly_mmsi,
                                      search_into_radius_m = search_into_radius_m,
                                      max_time_diff = 0,
                                      duplicate_time = F,
                                      t_gap = t_gap,
                                      average_at = average_at,
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
                    file = paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
            cat("\nFILE", paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"), "SAVED\n")
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
              file.exists(paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))) {

            cat("\nLOAD FILE", paste0(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"), "\n")

            ais_on_effort <- readRDS(file_AISextract_perHour, "_hour_", paste(unique(eff_h$hour_AIS_to_extract), collapse = "-"), ".rds"))
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

    # rm(ais_data_ref)
  } else {
    daily_ais <- ais_data
  }

  # rm(d_max)
  rm(ais_data)
  rm(eff_d)
  # rm(dates_ais)

  gc()
  cat("\n                                DONE                                  \n")

  return(daily_ais)

}
