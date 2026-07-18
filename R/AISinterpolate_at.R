#' Interpolate AIS data at the desired times.
#'
#' Interpolate AIS data at the desired times, after have corrected the GPS errors and delays (correcting the speed, distance and time travelled by the vessels), identified (and filter or not) the stations and aircraft, with possible time approximation if the computation time is too long. Contrary to AISinterpolate_at, you can here select what times to interpolate.
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param crs_meters projection (crs) in 'meters' to use to calculate distance over the study area. Default to 3035 (ETRS89).
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param load_existing_files if TRUE, load the existing files of AISinterpolate_at named as paste0(file_AISinterpolate_at, "_hour_", hour_processed, ".rds).
#' @param save_AISinterpolate_at if TRUE, save the results for each iteration of hour of AIS data (if run_AISinterpolate_at = T). IMPORTANT: The saved results should not be used outside of this function as they are not complete yet! Please re-run this function to have the full interpolated data.
#' @param overwrite if TRUE, the saved files (see save_AIStravel, save_AISinterpolate_at, save_AISextract_perHour) overwrite existing files. Otherwise load the existing files if these are existing and needed in the function.
#' @param file_AISinterpolate_at if save_AISinterpolate_at = TRUE, is the file name where hourly interpolated AIS data are saved. Must not contain file format: the files are written as .rds.
#' @param radius radius (meters) around data where AIS data are considered for interpolation of the positions. Must be large enough to collect the AIS data necessary for a linear interpolation at the time of the data. Is used also to filter the AIS data too far from the data of interest and slowing the processes (we used 200 km as default value of radius).
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param correct_speed if TRUE, GPS errors and GPS delays are identified and removed from AIS data. Vessel speeds, distance and time travelled are corrected. Usually necessary.
#' @param threshold_speed_to_correct threshold (km/h) above which speeds are considered as unrealistic and due to a GPS error or delay.
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions. The default expression has been tested as relevant and appropriate to filter GPS errors and delays, still checks are necessary.
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
#' \itemize{
#' \item id_ais_data_initial: identifier of the row in the ordered, corrected and cleaned ais data. Used for internal computation. For interpolated positions, id_ais_data_initial is the same than the next real existing AIS data.
#' \item station: if TRUE, the MMSI has been identified as a station.
#' \item high_speed: if TRUE, the MMSI has been identified as an high speed craft (specially used for aircraft).
#' \item any_NA_speed_kmh: if TRUE, at least one of the speeds of this MMSI has a speed as NA (so distance_travelled or time_travelled has a issue and the AIS data must be checked). Should not occur.
#' \item n_point_mmsi_initial_data: number of point of the MMSI in the AIS data after have removed the points with inexisting longitude and latitude.
#' \item id_mmsi_point_initial: identifier for the MMSI point after ordering, correcting and cleaning.
#' \item speed_kmh_corrected: if TRUE, the speed of the line has been corrected.
#' \item interpolated: if TRUE, this MMSI position has been interpolated.}
#'
#' @examples
#' \dontrun{
#' data("ais")
#' data("point_to_extract")
#'
#' library(dplyr)
#' library(lubridate)
#' point_to_extract <- point_to_extract %>%
#'   mutate(timestamp = as.numeric(ymd_hm(datetime)))
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             time_stop = 5*60*60,
#'             mmsi_time_to_order = T,
#'             return_sf = F,
#'             return_3035_coords = F)
#'
#' AISinterpolate_at(data = point_to_extract,
#'                   ais_data = ais,
#'                   mmsi_time_to_order = T,
#'                   load_existing_files = T,
#'                   save_AISinterpolate_at = T,
#'                   overwrite = F,
#'                   file_AISinterpolate_at = "AISinterpolate_at",
#'                   radius = 200000,
#'                   time_stop = 5*60*60,
#'                   correct_speed = T,
#'                   threshold_speed_to_correct = 100,
#'                   filter_station = T,
#'                   filter_high_speed = T,
#'                   parallelize = F)}
#' @export

AISinterpolate_at <- function(data,
                              ais_data,
                              crs_meters = 3035,
                              mmsi_time_to_order = TRUE,
                              load_existing_files = FALSE,
                              save_AISinterpolate_at = TRUE,
                              overwrite = FALSE,
                              file_AISinterpolate_at = "AISinterpolate_at",
                              radius = 200000,
                              time_stop = 5*60*60,
                              correct_speed = TRUE,
                              threshold_speed_to_correct = 100,
                              threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 1], na.rm = T) +
                                                                                               sd(speed_kmh[speed_kmh > 1 & speed_kmh < quantile(speed_kmh[speed_kmh > 1], .75)]) * 5 + 15))},
                              filter_station = TRUE,
                              filter_high_speed = TRUE,
                              quantile_station = 0.975,
                              threshold_distance_station = 1,
                              quantile_high_speed = 0.97,
                              threshold_high_speed = 110,
                              parallelize = FALSE,
                              nb_cores = NA,
                              outfile = "log.txt",
                              QUIET = FALSE
                              ){

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(data$lon))
  assertthat::assert_that(is.numeric(data$lat))
  assertthat::assert_that(is.numeric(data$timestamp))

  if (!parallelize) {
    parallelize <- T
    nb_cores <- 1
  }

  average_mmsi_at <- 0

  if (save_AISinterpolate_at & is.na(file_AISinterpolate_at)) {
    cat("save_AISinterpolate_at is TRUE but file_AISinterpolate_at is NA: change file name\n")
    file_AISinterpolate_at <- "NA"
  }

  timestamp_to_interpolate <- na.omit(unique(data$timestamp))

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    st_drop_geometry()

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
    AISidentify_stations_aircraft(ais_data = .,
                                  quantile_station = quantile_station,
                                  threshold_distance_station = threshold_distance_station,
                                  quantile_high_speed = quantile_high_speed,
                                  threshold_high_speed = threshold_high_speed)

  if (radius != Inf) {
    far <- which(ais_data$X >= (min(data$X, na.rm = T) - radius) & ais_data$X <= (max(data$X, na.rm = T) + radius) &
                   ais_data$Y >= (min(data$Y, na.rm = T) - radius) & ais_data$Y <= (max(data$Y, na.rm = T) + radius))

    if (length(far) > 0) {
      ais_data <- ais_data[far, ]
    }
    rm(far)
  }
  if (time_stop != Inf) {
    long <- ais_data$timestamp >= (min(timestamp_to_interpolate) - (time_stop + average_mmsi_at)) &
      ais_data$timestamp <= (max(timestamp_to_interpolate) + (time_stop + average_mmsi_at))

    if (length(long) > 0) {
      ais_data <- ais_data[long, ]
    }
    rm(long)
  }

  gc()

  if (filter_station) {
    lines <- which(ais_data$station)

    to_remove <- ais_data[lines,]

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

    if (length(lines) > 0) {
      ais_data <- ais_data[-lines,]
    }
    cat(length(lines), "data removed for HIGH SPEED, i.e.", length(unique(to_remove$mmsi)), "high speed engine\n")
    rm(to_remove)
    rm(lines)
  }

  gc()

  if (nrow(ais_data) > 0) {
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
                                   crs_meters = crs_meters,
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

    raverage_mmsi_at <- round(average_mmsi_at/2, 0)

    timestamp_averaged <- data.frame(t = timestamp_to_interpolate) %>%
      dplyr::group_by(t) %>%
      dplyr::reframe(t = (t-raverage_mmsi_at):(t+raverage_mmsi_at)) %>%
      arrange(t) %>%
      pull(t) %>%
      unique()

    ais_ok <- ais_data %>%
      dplyr::filter(timestamp %in% timestamp_averaged)

    to_run <- na.omit(purrr::map_dbl(timestamp_to_interpolate, function(t) {
      done <- ais_ok$mmsi[ais_ok$timestamp %in% (t-raverage_mmsi_at):(t+raverage_mmsi_at)]

      return(ifelse(all(ais_data$mmsi %in% done), NA, t))
    }))

    {
      all_to_run <- to_run
      hour_to_run <- unique(lubridate::hour(lubridate::as_datetime(all_to_run)))

      cl <- parallel::makeCluster(nb_cores, outfile = outfile)
      doParallel::registerDoParallel(cl)

      ais_data <- purrr::map_dfr(hour_to_run, function(hh) {

        if (overwrite | !load_existing_files |
            !(file.exists(paste0(file_AISinterpolate_at, "_hour_", hh, ".rds")))) {
          to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

          ais_datah <- ais_data

          if (time_stop != Inf) {
            ais_datah <- ais_datah[ais_datah$timestamp >= (min(to_run) - (time_stop + average_mmsi_at)) &
                                     ais_datah$timestamp <= (max(to_run) + (time_stop + average_mmsi_at)), ]
          }
          if (radius != Inf) {
            datah <- data[data$timestamp %in% to_run, ]

            datah <- add_coordinates_meters(datah, crs_meters = crs_meters) %>%
              st_drop_geometry() %>%
              distinct()

            ais_datah <- ais_datah[ais_datah$X >= (min(datah$X, na.rm = T) - radius) & ais_datah$X <= (max(datah$X, na.rm = T) + radius) &
                                     ais_datah$Y >= (min(datah$Y, na.rm = T) - radius) & ais_datah$Y <= (max(datah$Y, na.rm = T) + radius), ]
          }

          to_export <- unique(na.omit(c("QUIET", "datah", "ais_ok", "raverage_mmsi_at", "all_to_run",
                                        "ais_datah", "file_AISinterpolate_at", "radius", "time_stop", "average_mmsi_at",
                                        "hh"
          )))

          out <- foreach::foreach(t = to_run,
                                  .export = to_export,
                                  .noexport = c("data", "ais_data"),
                                  .packages = c("dplyr","tidyverse", "sf")
          ) %dopar% {
            if (!QUIET) {
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
                  sf::st_transform(crs = crs_meters)

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

          if (save_AISinterpolate_at & (!file.exists(paste0(file_AISinterpolate_at, "_hour_", hh, ".rds")) | overwrite)) {
            saveRDS(out, paste0(file_AISinterpolate_at, "_hour_", hh, ".rds"))
            cat("\nFILE", paste0(file_AISinterpolate_at, "_hour_", hh, ".rds"), "SAVED\n")
          }

          gc()
        } else {
          cat("\nLOAD FILE", paste0(file_AISinterpolate_at, "_hour_", hh, ".rds"), "\n")
          out <- readRDS(paste0(file_AISinterpolate_at, "_hour_", hh, ".rds"))
        }

        return(out)
      })

      parallel::stopCluster(cl)
      gc()

    }

    out <- purrr::map_dfr(list(ais_ok,
                               ais_data),
                          function(d) {return(d)}) %>%
      dplyr::distinct()

    out <- AIStravel(ais_data = out %>%
                       dplyr::select(-c(time_travelled, distance_travelled, speed_kmh)),
                     crs_meters = crs_meters,
                     time_stop = time_stop,
                     mmsi_time_to_order = T,
                     return_3035_coords = ifelse("X" %in% colnames(out) & "Y" %in% colnames(out),
                                                 T,
                                                 F))

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

    return(out)
  } else {

    cat("No point left in AIS data after filtering for stations, high speed, time stop and radius.\n")
    return(NULL)

  }

}
