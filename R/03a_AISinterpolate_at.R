#' Interpolate AIS data at the desired times.
#'
#' Interpolate AIS data at the desired times, after have corrected the GPS errors and delays (correcting the speed, distance and time travelled by the vessels), identified (and filter or not) the stations and aircraft, with possible time approximation if the computation time is too long.
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param load_existing_files if TRUE, load the existing files of AISinterpolate_at named as paste0(file_AISinterlate_at, "_hour_", hour_processed, ".rds).
#' @param save_AISinterlate_at if TRUE, save the results for each iteration of hour of AIS data (if run_AISinterpolate_at = T)
#' @param overwrite if TRUE, the saved files (see save_AIStravel, save_AISinterlate_at, save_AISextract_perHour) overwrite existing files. Otherwise load the existing files if these are existing and needed in the function.
#' @param file_AISinterlate_at if save_AISinterlate_at = TRUE, is the file name where hourly interpolated AIS data are saved. Must not contain file format: the files are written as .rds.
#' @param radius radius (kilometers) around data where AIS data are considered for interpolation of the positions. Must be large enough to collect the AIS data necessary for a linear interpolation at the time of the data. Is used also to filter the AIS data too far from the data of interest and slowing the processes (we used 200 km as default value of radius).
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param correct_speed if TRUE, GPS errors and GPS delays are identified and removed from AIS data. Vessel speeds, distance and time travelled are corrected. Usually necessary.
#' @param threshold_speed_to_correct threshold (km/h) above which speeds are considered as unrealistic and due to a GPS error or delay.
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions.
#' @param filter_station if TRUE, filter the stations out.
#' @param filter_high_speed if TRUE, filter the aircraft out.
#' @param quantile_station Quantile (0 to 1) of distance, by mmsi, which is compared to threshold_distance_station to assess if the MMSI is a station or not: if below threshold_distance_station, MMSI is considered as stationary and is a station. We used 0.975 to prevent misinterpretations from GPS errors leading to distance travelled by stations.
#' @param threshold_distance_station Threshold of distance (meters) used to assess if the MMSI is a station.
#' @param quantile_high_speed Quantile (0 to 1) of speed, by mmsi, which is compared to threshold_high_speed to assess if the MMSI is a aircraft or not: if above threshold_high_speed, MMSI is considered as a station. We used 0.97 to prevent misinterpretations from GPS errors.
#' @param threshold_high_speed Threshold of speed (km/h) used to assess if the MMSI is an aircraft.
#' @param parallelize if TRUE, parallelize with "doParallel" package the processes (required powerful computer if large AIS dataset and data timestamps to process.)
#' @param nb_cores number of cores to used with doParallel.
#' @param outfile file to print the logs if parallelize = T.
#' @param QUIET if TRUE, print the iterations: either in the console if parallelize = F, or in the file "outfile" if parallelize = T.
#'
#' @return return AIS data at the timestamps desired, interpolated (or not). Contains the columns:
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
AISinterpolate_at <- function(data,
                              ais_data,
                              mmsi_time_to_order = T,
                              load_existing_files = F,
                              save_AISinterlate_at = T,
                              overwrite = F,
                              file_AISinterlate_at = "AISinterpolate_at",
                              radius = 200000,
                              time_stop = 5*60*60,
                              correct_speed = T,
                              threshold_speed_to_correct = 100,
                              threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 0], na.rm = T) +
                                                                                               sd(speed_kmh[speed_kmh > 0 & speed_kmh < quantile(speed_kmh, 0.75, na.rm = T)])*2.5 + 15))},
                              # average_mmsi_at = 0,
                              filter_station = T,
                              filter_high_speed = T,
                              quantile_station = 0.975,
                              threshold_distance_station = 10,
                              quantile_high_speed = 0.97,
                              threshold_high_speed = 110,
                              # interpolate_station = T,
                              # interpolate_high_speed = T,
                              parallelize = F,
                              nb_cores = NA,
                              outfile = "log.txt",
                              QUIET = F#,
                              # spatial_limit = NA, no tests perform on these 3 parameters of land and limits + interaction with return_all ?
                              # on_Land_analysis = F,
                              # land_sf_polygon = NA,
                              # return_all = F ## issue with id_ais_initial_data, that an column with initial values must be created and taken back at the end to identify duplicates from interpolated.
){

  # param spatial_limit sf polygon object of the area where outside points must be filtered out of the output. Not tested and might lead to few errors.
  # param on_Land_analysis sf polygon object of the countries to study the reliability of GPS positions and interpolations with an analysis of the paths travelled by mmsi on land. Not tested and might lead to few errors.
  # param land_sf_polygon if on_Land_analysis, sf polygon object for countries.
  # param return_all if all AIS data must be returned, after ordering, correction, cleaning and interpolation, or only interpolated positions of the vessels at the time desired (smaller dataset).
  # param average_mmsi_at number of seconds where positions of mmsi are averaged for extraction. Less useful than average_at which average the data timestamps to process.
  average_mmsi_at <- 0
  # param interpolate_station if FALSE, do not interpolate the positions of the stations.
  # param interpolate_high_speed if FALSE, do not interpolate the positions of the aircrafts.
  # interpolate_station <- ifelse()

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  if (save_AISinterlate_at & is.na(file_AISinterlate_at)) {
    cat("save_AISinterlate_at is TRUE but file_AISinterlate_at is NA: change file name\n")
    file_AISinterlate_at <- "NA"
  }

  timestamp_to_interpolate <- na.omit(unique(data$timestamp))

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
      dplyr::arrange(timestamp)
  }

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n())

  ais_data <- ais_data %>%
    dplyr::mutate(inside_temp = ifelse(timestamp >= min(data$timestamp, na.rm = T) &
                                         timestamp <= max(data$timestamp, na.rm = T), T, F),
                  before_temp = ifelse(timestamp < min(data$timestamp, na.rm = T), T, F),
                  after_temp = ifelse(timestamp > max(data$timestamp, na.rm = T), T, F))

  ais_data <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) <= threshold_distance_station, T, F),
                  high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) >= threshold_high_speed, T, F),
                  any_NA_speed_kmh = ifelse(any(is.na(speed_kmh)), T, F),
                  n_point_mmsi_initial_data = n(),
                  id_mmsi_point_initial = 1:n()) %>%
    ungroup()

  # if (return_all) {
  #   ais_data_ref <- ais_data %>%
  #     dplyr::mutate(speed_kmh_corrected = F) %>%
  #     dplyr::select(!c("inside_temp", "before_temp", "after_temp"))
  # }

  if (radius != Inf) {
    ais_data <- ais_data[ais_data$X >= (min(data$X, na.rm = T) - radius) & ais_data$X <= (max(data$X, na.rm = T) + radius) &
                           ais_data$Y >= (min(data$Y, na.rm = T) - radius) & ais_data$Y <= (max(data$Y, na.rm = T) + radius), ]
  }
  if (time_stop != Inf) {
    ais_data <- ais_data[ais_data$timestamp >= (min(timestamp_to_interpolate) - (time_stop + average_mmsi_at)) &
                           ais_data$timestamp <= (max(timestamp_to_interpolate) + (time_stop + average_mmsi_at)), ]
  }

  gc()

  if (filter_station) {
    lines <- which(ais_data$station)

    to_remove <- ais_data[lines,]

    # if (return_all) {
    #   ais_data_ref <- ais_data_ref %>%
    #     dplyr::filter(!(mmsi %in% unique(to_remove$mmsi)))
    # }

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }

    cat(length(lines), "data removed for STATION, i.e.", length(unique(to_remove$mmsi)), "stations\n")
    rm(to_remove)
    rm(lines)
  }
  if (filter_high_speed) {
    lines <- which(ais_data$high_speed)

    to_remove <- ais_data[lines,]

    # if (return_all) {
    #   ais_data_ref <- ais_data_ref %>%
    #     dplyr::filter(!(mmsi %in% unique(to_remove$mmsi)))
    # }

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }
    cat(length(lines), "data removed for HIGH SPEED, i.e.", length(unique(to_remove$mmsi)), "high speed engine\n")
    rm(to_remove)
    rm(lines)
  }

  gc()

  ais_data <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(id_mmsi_point_initiali = id_mmsi_point_initial,
                  id_mmsi_point_initial = 1:n()) %>%
    ungroup()

  ais_data <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::filter(id_mmsi_point_initial %in% ifelse(any(inside_temp),
                                                    id_mmsi_point_initial[max(2, dplyr::first(which(inside_temp))) - 1],
                                                    ifelse(any(before_temp),
                                                           id_mmsi_point_initial[dplyr::last(which(before_temp))],
                                                           1)):ifelse(any(inside_temp),
                                                                      id_mmsi_point_initial[min(n() - 1, dplyr::last(which(inside_temp))) + 1],
                                                                      ifelse(any(after_temp),
                                                                             id_mmsi_point_initial[dplyr::first(which(after_temp))],
                                                                             n()))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(id_mmsi_point_initial = id_mmsi_point_initiali) %>%
    dplyr::select(!c("inside_temp", "before_temp", "after_temp", "id_mmsi_point_initiali"))

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)
  }

  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:n()) %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(id_mmsi_point_initial_real = id_mmsi_point_initial,
                  id_mmsi_point_initial = 1:n()) %>%
    dplyr::ungroup()

  if (correct_speed) {
    cat("   --> Correct speeds\n")

    ais_data <- AIScorrect_speed(ais_data = ais_data,
                                 mmsi_time_to_order = F,
                                 correct_high_speed_craft = F,
                                 threshold_speed_to_correct = threshold_speed_to_correct,
                                 threshold_speed_to_correct_expr = threshold_speed_to_correct_expr,
                                 time_stop = time_stop)

  }

  raverage_mmsi_at <- round(average_mmsi_at/2, 0)
  timestamp_averaged <- unique(do.call("c", purrr::map(timestamp_to_interpolate, function(t) {
    return((t-raverage_mmsi_at):(t+raverage_mmsi_at))
  })))

  ais_ok <- ais_data %>%
    dplyr::filter(timestamp %in% timestamp_averaged)

  to_run <- na.omit(purrr::map_int(timestamp_to_interpolate, function(t) {
    done <- ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)]

    return(ifelse(all(ais_data$mmsi %in% done), NA, t))
  }))

  # d_max <- (radius*1.5 + 2000) / (8*1e3)

  # if (any(!is.na(spatial_limit))) {
  #   if (sf::st_crs(spatial_limit)$input != "EPSG:3035") {
  #     spatial_limit <- spatial_limit %>%
  #       sf::st_transform(crs = 3035)
  #   }
  # }
  # if (any(!is.na(land_sf_polygon))) {
  #   if (sf::st_crs(land_sf_polygon)$input != "EPSG:3035") {
  #     land_sf_polygon <- land_sf_polygon %>%
  #       sf::st_transform(crs = 3035)
  #   }
  # }

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

    ais_data <- purrr::map_dfr(hour_to_run, function(hh) {

      if (overwrite | !load_existing_files |
          !(file.exists(paste0(file_AISinterlate_at, "_hour_", hh, ".rds"))))  {
        to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

        ais_datah <- ais_data

        if (time_stop != Inf) {
          ais_datah <- ais_datah[ais_datah$timestamp >= (min(to_run) - (time_stop + average_mmsi_at)) &
                                   ais_datah$timestamp <= (max(to_run) + (time_stop + average_mmsi_at)), ]
        }
        if (radius != Inf) {
          datah <- data[data$timestamp %in% to_run, ]

          if (!(all(c("X", "Y") %in% colnames(datah)))) {
            if (!("sf" %in% class(datah))) {
              datah <- datah %>%
                dplyr::select(c("lon", "lat")) %>%
                dplyr::distinct() %>%
                sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
            }
            if (st_crs(datah)$input != "EPSG:3035") {
              datah <- datah %>%
                sf::st_transform(crs = 3035)
            }

            datah <- datah %>%
              sf::st_coordinates() %>%
              as.data.frame()
          }

          ais_datah <- ais_datah[ais_datah$X >= (min(datah$X, na.rm = T) - radius) & ais_datah$X <= (max(datah$X, na.rm = T) + radius) &
                                   ais_datah$Y >= (min(datah$Y, na.rm = T) - radius) & ais_datah$Y <= (max(datah$Y, na.rm = T) + radius), ]
        }

        out <- purrr::map_dfr(to_run, function(t) {
          if (!QUIET) {
            utils::setTxtProgressBar(pb, match(t, all_to_run))
          }

          done <- unique(ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)])

          temp <- ais_datah

          if (length(done) > 0) {
            temp <- temp[!(temp$mmsi %in% done), ]
          }
          if (time_stop != Inf) {
            temp <- temp[temp$timestamp >= (t - (time_stop + average_mmsi_at)) &
                           temp$timestamp <= (t + (time_stop + average_mmsi_at)), ]
          }
          if (radius != Inf) {
            data_coords <- datah[datah$timestamp == t, ]

            # if (!(all(c("X", "Y") %in% colnames(data_coords)))) {
            #   if (!("sf" %in% class(data_coords))) {
            #     data_coords <- data_coords %>%
            #       dplyr::select(c("lon", "lat")) %>%
            #       dplyr::distinct() %>%
            #       sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
            #   }
            #   if (st_crs(data_coords)$input != "EPSG:3035") {
            #     data_coords <- data_coords %>%
            #       sf::st_transform(crs = 3035)
            #   }
            #
            #   data_coords <- data_coords %>%
            #     sf::st_coordinates() %>%
            #     as.data.frame()
            # }

            temp <- temp[temp$X >= (min(data_coords$X, na.rm = T) - radius) & temp$X <= (max(data_coords$X, na.rm = T) + radius) &
                           temp$Y >= (min(data_coords$Y, na.rm = T) - radius) & temp$Y <= (max(data_coords$Y, na.rm = T) + radius), ]
            rm(data_coords)
          }

          temp$difftimestamp <- temp$timestamp - t

          rm(done)

          sup <- temp[temp$difftimestamp > 0, ] %>%
            dplyr::group_by(mmsi) %>%
            dplyr::slice_min(difftimestamp) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          inf <- temp[temp$difftimestamp < 0, ] %>%
            dplyr::group_by(mmsi) %>%
            dplyr::slice_max(difftimestamp) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          rm(temp)

          to_interp <- rbind(inf, sup)

          if (nrow(to_interp) > 0) {
            n_point <- table(to_interp$mmsi)
            m_to_interp <- names(n_point)[n_point == 2]

            out_ok <- to_interp %>%
              dplyr::filter(mmsi %in% names(n_point)[n_point == 1])

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
                cat("\nCHECK THE CODE FOR tmmsi == mmsi\n")
                interp <- interp %>%
                  dplyr::filter(tmmsi == mmsi)
              }

              rm(prec)

              # if (on_Land_analysis) {
              #   interp <- interp %>%
              #     dplyr::group_by(id_ais_data_initial) %>%
              #     dplyr::reframe(time_travelled = c(0, t - ttimestamp, timestamp - t),
              #                    timestamp = c(ttimestamp, t, timestamp),
              #                    speed_kmh = unique(speed_kmh),
              #                    interpolated = c(F, T, F),
              #                    distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
              #                    lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
              #                    lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
              #     )
              #
              #   interp_eez <- interp %>%
              #     dplyr::left_join(interp_ref %>%
              #                 dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat", "X", "Y")),
              #               by = "id_ais_data_initial") %>%
              #     dplyr::mutate(id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = ".")))
              # } else {
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
              # }

              rm(to_interp)
              rm(interp)
              rm(interp_ref)

              interp_eez <- interp_eez %>%
                dplyr::mutate(tlon = lon,
                              tlat = lat) %>%
                sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
                sf::st_transform(crs = 3035)

              # if (!is.na(spatial_limit)) {
              #   to_keep <- colnames(interp_eez)
              #
              #   interp_eez_int <- interp_eez %>%
              #     dplyr::filter(interpolated)
              #
              #   interp_eez <- interp_eez_int[sf::st_intersects(interp_eez_int, spatial_limit, sparse = F), ] %>%
              #     sf::st_drop_geometry() %>%
              #     dplyr::select(all_of(to_keep)) %>%
              #     rbind(interp_eez %>%
              #             dplyr::filter(!interpolated))
              #
              #   rm(interp_eez_int)
              #   rm(to_keep)
              # }

              # if (on_Land_analysis)  {
              #   ######### to check on land,
              #   interp_eez <- interp_eez %>%
              #     dplyr::mutate(on_Land = c(ifelse(sf::st_intersects(., land_sf_polygon, sparse = F) == F, F, T)))
              #
              #   interp_eez_Land <- interp_eez %>%
              #     dplyr::group_by(mmsi) %>%
              #     dplyr::summarise(do_union = F) %>%
              #     sf::st_cast("LINESTRING") %>%
              #     dplyr::ungroup() %>%
              #     dplyr::mutate(total_distance_interp = units::drop_units(sf::st_length(.))) %>%
              #     sf::st_intersection(., land_sf_polygon) %>%
              #     dplyr::mutate(distance_interp_on_land = units::drop_units(sf::st_length(.))) %>%
              #     st_drop_geometry()
              #
              #   interp_eez <- interp_eez %>%
              #     dplyr::left_join(interp_eez_Land %>%
              #                 dplyr::select(distance_interp_on_land, total_distance_interp, id_ais_data_initial) %>%
              #                   dplyr::group_by(id_ais_data_initial) %>%
              #                 dplyr::summarise(total_distance_interp = unique(total_distance_interp),
              #                                  distance_interp_on_land = sum(distance_interp_on_land)),
              #               by = "id_ais_data_initial")
              #
              #   interp_eez <- interp_eez %>%
              #     dplyr::filter(interpolated)
              #
              #   rm(interp_eez_Land)
              # }

              coords_ais <- interp_eez %>%
                sf::st_coordinates() %>%
                as.data.frame()

              interp_eez <- interp_eez %>%
                sf::st_drop_geometry() %>%
                dplyr::mutate(X = coords_ais[,1],
                              Y = coords_ais[,2])

              out <- purrr::map_dfr(list(out_ok,
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

        if (save_AISinterlate_at & (!file.exists(paste0(file_AISinterlate_at, "_hour_", hh, ".rds")) | overwrite)) {
          saveRDS(out, paste0(file_AISinterlate_at, "_hour_", hh, ".rds"))
          cat("\nFILE", paste0(file_AISinterlate_at, "_hour_", hh, ".rds"), "SAVED\n")
        }

        gc()
      } else {
        cat("\nLOAD FILE", paste0(file_AISinterlate_at, "_hour_", hh, ".rds"), "\n")
        out <- readRDS(paste0(file_AISinterlate_at, "_hour_", hh, ".rds"))
        utils::setTxtProgressBar(pb, match(dplyr::last(to_run), all_to_run))
      }

      return(out)
    })
  } else {
    all_to_run <- to_run
    hour_to_run <- unique(lubridate::hour(lubridate::as_datetime(all_to_run)))

    cl <- parallel::makeCluster(nb_cores, outfile = outfile)
    doParallel::registerDoParallel(cl)

    ais_data <- purrr::map_dfr(hour_to_run, function(hh) {

      if (overwrite | !load_existing_files |
          !(file.exists(paste0(file_AISinterlate_at, "_hour_", hh, ".rds")))) {
        to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

        ais_datah <- ais_data

        if (time_stop != Inf) {
          ais_datah <- ais_datah[ais_datah$timestamp >= (min(to_run) - (time_stop + average_mmsi_at)) &
                                   ais_datah$timestamp <= (max(to_run) + (time_stop + average_mmsi_at)), ]
        }
        if (radius != Inf) {
          datah <- data[data$timestamp %in% to_run, ]

          if (!(all(c("X", "Y") %in% colnames(datah)))) {
            if (!("sf" %in% class(datah))) {
              datah <- datah %>%
                dplyr::select(c("lon", "lat")) %>%
                dplyr::distinct() %>%
                sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
            }
            if (st_crs(datah)$input != "EPSG:3035") {
              datah <- datah %>%
                sf::st_transform(crs = 3035)
            }

            datah <- datah %>%
              sf::st_coordinates() %>%
              as.data.frame()
          }

          ais_datah <- ais_datah[ais_datah$X >= (min(datah$X, na.rm = T) - radius) & ais_datah$X <= (max(datah$X, na.rm = T) + radius) &
                                   ais_datah$Y >= (min(datah$Y, na.rm = T) - radius) & ais_datah$Y <= (max(datah$Y, na.rm = T) + radius), ]
        }

        to_export <- unique(c(ls(),  na.omit(c("QUIET", "datah", "ais_ok", "raverage_mmsi_at", "all_to_run",
                                               "ais_datah", "file_AISinterlate_at", "radius", "time_stop", "average_mmsi_at",
                                               "hh", ifelse(!interpolate_station, "list_station", NA), ifelse(!interpolate_high_speed, "list_high_speed", NA),
                                               "interpolate_station", "interpolate_high_speed"))))

        out <- foreach::foreach(t = to_run,
                                .export = to_export,
                                .noexport = c("data", "ais_data"),
                                .packages = c("dplyr","tidyverse", "sf")
        ) %dopar% {
          if (!QUIET) {
            # setTxtProgressBar(pb, match(t, timestamp_to_interpolate))
            # print(t)
            cat(match(t, all_to_run), "/", length(all_to_run), "\n")

          }

          done <- unique(ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)])

          temp <- ais_datah

          if (length(done) > 0) {
            temp <- temp[!(temp$mmsi %in% done), ]
          }
          if (time_stop != Inf) {
            temp <- temp[temp$timestamp >= (t - (time_stop + average_mmsi_at)) &
                           temp$timestamp <= (t + (time_stop + average_mmsi_at)), ]
          }
          if (radius != Inf) {
            data_coords <- datah[datah$timestamp == t, ]

            # if (!(all(c("X", "Y") %in% colnames(data_coords)))) {
            #   if (!("sf" %in% class(data_coords))) {
            #     data_coords <- data_coords %>%
            #       dplyr::select(c("lon", "lat")) %>%
            #       dplyr::distinct() %>%
            #       sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
            #   }
            #   if (st_crs(data_coords)$input != "EPSG:3035") {
            #     data_coords <- data_coords %>%
            #       sf::st_transform(crs = 3035)
            #   }
            #
            #   data_coords <- data_coords %>%
            #     sf::st_coordinates() %>%
            #     as.data.frame()
            # }

            temp <- temp[temp$X >= (min(data_coords$X, na.rm = T) - radius) & temp$X <= (max(data_coords$X, na.rm = T) + radius) &
                           temp$Y >= (min(data_coords$Y, na.rm = T) - radius) & temp$Y <= (max(data_coords$Y, na.rm = T) + radius), ]
            rm(data_coords)
          }

          temp$difftimestamp <- temp$timestamp - t

          rm(done)

          sup <- temp[temp$difftimestamp > 0, ] %>%
            dplyr::group_by(mmsi) %>%
            dplyr::slice_min(difftimestamp) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          inf <- temp[temp$difftimestamp < 0, ] %>%
            dplyr::group_by(mmsi) %>%
            dplyr::slice_max(difftimestamp) %>%
            dplyr::ungroup() %>%
            dplyr::filter(!duplicated(mmsi))

          rm(temp)

          to_interp <- rbind(inf, sup)

          if (nrow(to_interp) > 0) {
            n_point <- table(to_interp$mmsi)
            m_to_interp <- names(n_point)[n_point == 2]

            out_ok <- to_interp %>%
              dplyr::filter(mmsi %in% names(n_point)[n_point == 1])

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
                cat("\nCHECK THE CODE FOR tmmsi == mmsi\n")
                interp <- interp %>%
                  dplyr::filter(tmmsi == mmsi)
              }

              rm(prec)

              # if (on_Land_analysis) {
              #   interp <- interp %>%
              #     dplyr::group_by(id_ais_data_initial) %>%
              #     dplyr::reframe(time_travelled = c(0, t - ttimestamp, timestamp - t),
              #                    timestamp = c(ttimestamp, t, timestamp),
              #                    speed_kmh = unique(speed_kmh),
              #                    interpolated = c(F, T, F),
              #                    distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
              #                    lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
              #                    lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
              #     )
              #
              #   interp_eez <- interp %>%
              #     dplyr::left_join(interp_ref %>%
              #                 dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat", "X", "Y")),
              #               by = "id_ais_data_initial") %>%
              #     dplyr::mutate(id_ais_data_initial = as.numeric(paste(id_ais_data_initial, "1", sep = ".")))
              # } else {
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
              # }

              rm(to_interp)
              rm(interp)
              rm(interp_ref)

              interp_eez <- interp_eez %>%
                dplyr::mutate(tlon = lon,
                              tlat = lat) %>%
                sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
                sf::st_transform(crs = 3035)

              # if (!is.na(spatial_limit)) {
              #   to_keep <- colnames(interp_eez)
              #
              #   interp_eez_int <- interp_eez %>%
              #     dplyr::filter(interpolated)
              #
              #   interp_eez <- interp_eez_int[sf::st_intersects(interp_eez_int, spatial_limit, sparse = F), ] %>%
              #     sf::st_drop_geometry() %>%
              #     dplyr::select(dplyr::all_of(to_keep)) %>%
              #     rbind(interp_eez %>%
              #             dplyr::filter(!interpolated))
              #
              #   rm(interp_eez_int)
              #   rm(to_keep)
              # }

              # if (on_Land_analysis)  {
              #   ######### to check on land,
              #   interp_eez <- interp_eez %>%
              #     dplyr::mutate(on_Land = c(ifelse(sf::st_intersects(., land_sf_polygon, sparse = F) == F, F, T)))
              #
              #   interp_eez_Land <- interp_eez %>%
              #     dplyr::group_by(mmsi) %>%
              #     dplyr::summarise(do_union = F) %>%
              #     sf::st_cast("LINESTRING") %>%
              #     dplyr::ungroup() %>%
              #     dplyr::mutate(total_distance_interp = units::drop_units(sf::st_length(.))) %>%
              #     sf::st_intersection(., land_sf_polygon) %>%
              #     dplyr::mutate(distance_interp_on_land = units::drop_units(sf::st_length(.))) %>%
              #     sf::st_drop_geometry()
              #
              #   interp_eez <- interp_eez %>%
              #     dplyr::left_join(interp_eez_Land %>%
              #                 dplyr::select(distance_interp_on_land, total_distance_interp, id_ais_data_initial) %>%
              #                   dplyr::group_by(id_ais_data_initial) %>%
              #                 dplyr::summarise(total_distance_interp = unique(total_distance_interp),
              #                                  distance_interp_on_land = sum(distance_interp_on_land)),
              #               by = "id_ais_data_initial")
              #
              #   interp_eez <- interp_eez %>%
              #     dplyr::filter(interpolated)
              #
              #   rm(interp_eez_Land)
              # }

              coords_ais <- interp_eez %>%
                sf::st_coordinates() %>%
                as.data.frame()

              interp_eez <- interp_eez %>%
                sf::st_drop_geometry() %>%
                dplyr::mutate(X = coords_ais[,1],
                              Y = coords_ais[,2])

              out <- purrr::map_dfr(list(out_ok,
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

        out <- purrr::map_dfr(out, function(d) {return(d)})

        if (save_AISinterlate_at & (!file.exists(paste0(file_AISinterlate_at, "_hour_", hh, ".rds")) | overwrite)) {
          saveRDS(out, paste0(file_AISinterlate_at, "_hour_", hh, ".rds"))
          cat("\nFILE", paste0(file_AISinterlate_at, "_hour_", hh, ".rds"), "SAVED\n")
        }

        gc()
      } else {
        cat("\nLOAD FILE", paste0(file_AISinterlate_at, "_hour_", hh, ".rds"), "\n")
        out <- readRDS(paste0(file_AISinterlate_at, "_hour_", hh, ".rds"))
      }

      return(out)
    })

    parallel::stopCluster(cl)
    gc()

  }

  # rm(cols)

  out <- purrr::map_dfr(list(ais_ok,
                             ais_data %>%
                               dplyr::distinct()),
                        function(d) {return(d)})

  # if (return_all) {
  #   out <- purrr::map_dfr(list(out,
  #                       ais_data_ref %>%
  #                         dplyr::filter(!(id_ais_data_initial %in% out$id_ais_data_initial))), function(x) {return(x)})
  # }

  out <- out  %>%
    dplyr::arrange(mmsi, timestamp)

  if (!("interpolated") %in% colnames(out)) {
    out <- out %>%
      dplyr::mutate(interpolated = NA)
  }

  out <- out %>%
    dplyr::mutate(datetime = as.character(lubridate::as_datetime(timestamp)),
                  date = as.character(lubridate::date(datetime)),
                  interpolated = ifelse(is.na(interpolated), F, interpolated))

  if ("id_mmsi_point_initial_real" %in% colnames(out)) {
    out <- out %>%
      dplyr::mutate(id_mmsi_point_initial = id_mmsi_point_initial_real) %>%
      dplyr::select(-id_mmsi_point_initial_real)
  }

  rm(ais_ok)
  rm(ais_data)
  # rm(ais_data_ref)

  return(out)

}
