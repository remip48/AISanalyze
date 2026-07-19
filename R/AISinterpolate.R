#' Interpolate AIS positions
#'
#' Interpolates vessel positions at regular time intervals or at
#' user-defined timestamps. Interpolation can optionally be restricted to
#' a given radius withing target locations
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric.
#' @param type_interpolation Interpolation mode: `"maximum_time_interval"` or
#'   `"exact_timestamp"`.
#' @param maximum_time_interval List used when
#'   `type_interpolation = "maximum_time_interval"`, containing
#'   `maximum_gap_seconds`.
#' @param exact_timestamp List used when
#'   `type_interpolation = "exact_timestamp"`, containing:
#'   \itemize{
#'   \item `timestamp_to_interpolate`: Target timestamps.
#'   \item `locations_of_interest`: Optional data frame with `lon` and `lat` columns
#'     corresponding to each `timestamp_to_interpolate`.
#'   \item `radius`: Optional search radius (m) around each target location.
#'   }
#' @param crs_meters CRS (in metres) used for distance calculations. Defaults to
#'   EPSG:3035.
#' @param parallelize Logical. If `TRUE`, interpolation is parallelized using
#'   **doParallel**.
#' @param nb_cores Number of CPU cores used when `parallelize = TRUE`.
#' @param outfile File used to save logs when `parallelize = TRUE`.
#'
#' @return The interpolated AIS data with an additional column:
#' \itemize{
#' \item `interpolated`: Whether the position was interpolated.
#' }
#'
#' @examples
#' \dontrun{
#' library(AISanalyze)
#' data("ais")
#' data("point_to_extract")
#'
#' point_to_extract <- point_to_extract %>%
#'   mutate(timestamp = as.numeric(ymd_hm(datetime)))
#'
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             return_sf = F,
#'             return_meter_coords = F)
#'
#' # to interpolate all vessel locations separated by > 60 seconds
#' out <- AISinterpolate(ais_data = ais,
#'                type_interpolation = "maximum_time_interval",
#'                maximum_time_interval = list(maximum_gap_seconds = 60),
#'                crs_meters = 3035,
#'                parallelize = F)
#'
#' # to interpolate all vessel locations at exact timestamps, within a radius of 200 000 meters around
#' # target locations
#' out <- AISinterpolate(ais_data = ais,
#'                type_interpolation = "exact_timestamp",
#'                exact_timestamp = list(timestamp_to_interpolate = point_to_extract$timestamp,
#'                                       locations_of_interest = data.frame(lon = point_to_extract$lon,
#'                                                                          lat = point_to_extract$lat),
#'                                       radius = 200000),
#'                crs_meters = 3035,
#'                parallelize = F)}
#' @export

AISinterpolate <- function(ais_data,
                              type_interpolation,
                              maximum_time_interval = list(maximum_gap_seconds = 10 * 60),
                              exact_timestamp = list(timestamp_to_interpolate = NULL,
                                                     locations_of_interest = NULL,
                                                     radius = 200000),
                              crs_meters = 3035,
                              parallelize = FALSE,
                              nb_cores = NA,
                              outfile = "log.txt"
){

  ## set up the parameters of interpolation according to the type of interpolation
  if (type_interpolation == "exact_timestamp") {

    locations_of_interest <- exact_timestamp$locations_of_interest
    timestamp_to_interpolate <- exact_timestamp$timestamp_to_interpolate
    radius <- exact_timestamp$radius

    if (all(is.null(locations_of_interest))) {
      cat("All AIS data will be interpolated at `timestamp_to_interpolate`. If you want to target a specific
          region, use `locations_of_interest` argument.")
    } else {
      assertthat::assert_that(ncol(locations_of_interest) == 2)
      assertthat::assert_that(nrow(locations_of_interest) == length(timestamp_to_interpolate))
    }

    data <- data.frame(timestamp = timestamp_to_interpolate,
                       lon = locations_of_interest[,1],
                       lat = locations_of_interest[,2]) %>%
      dplyr::mutate(lon = ifelse(is.null(lon), 0, lon),
                    lat = ifelse(is.null(lat), 0, lat))

    timestamp_to_interpolate <- sort(unique(timestamp_to_interpolate))
  } else if (type_interpolation == "maximum_time_interval") {
    data <- data.frame(timestamp = c(-Inf, Inf),
                       lon = c(0, 0),
                       lat = c(0, 0))

    radius <- Inf

    maximum_gap_seconds <- maximum_time_interval$maximum_gap_seconds

    timestamp_to_interpolate <- c(-Inf, Inf)
  } else {
    stop("'type_interpolation' must be either 'maximum_time_interval' or 'exact_timestamp'")
  }

  assertthat::assert_that(is.numeric(ais_data$lon))
  assertthat::assert_that(is.numeric(ais_data$lat))
  assertthat::assert_that(is.numeric(ais_data$timestamp))
  assertthat::assert_that(is.numeric(data$lon))
  assertthat::assert_that(is.numeric(data$lat))
  assertthat::assert_that(is.numeric(data$timestamp))

  if (!parallelize) {
    parallelize <- T
    nb_cores <- 1
  } else if (is.na(nb_cores)) {
    nb_cores <- parallel::detectCores() * 3 / 4 %>%
      round(., 0)
  }

  initial_columns <- colnames(ais_data)

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    st_drop_geometry()

  ais_data <- add_coordinates_meters(ais_data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(timestamp >= (min(timestamp_to_interpolate) - (5*60*60)) &
                    timestamp <= (max(timestamp_to_interpolate) + (5*60*60))) %>%
    dplyr::filter(X >= (min(data$X, na.rm = T) - radius) & X <= (max(data$X, na.rm = T) + radius) &
                    Y >= (min(data$Y, na.rm = T) - radius) & Y <= (max(data$Y, na.rm = T) + radius))

  gc()

  if (nrow(ais_data) > 0) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)

    if (type_interpolation == "maximum_time_interval") {
      ais_data <- ais_data %>%
        dplyr::mutate(id_ais_data_initial = 1:n())

      to_interp <- ais_data %>%
        dplyr::group_by(mmsi) %>%
        dplyr::mutate(idd = 1:n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(idd != 1)  %>%
        dplyr::select(-idd)%>%
        dplyr::filter(time_travelled > maximum_gap_seconds) %>%
        dplyr::filter(time_travelled < 5*60*60)

      prec <- ais_data[to_interp$id_ais_data_initial - 1, ]

      interp <- to_interp %>%
        dplyr::mutate(ttimestamp = prec$timestamp,
                      tmmsi = prec$mmsi,
                      tlon = prec$lon,
                      tlat = prec$lat)

      rm(prec)

      interp <- interp[interp$mmsi == interp$tmmsi, ] %>%
        dplyr::group_by(id_ais_data_initial) %>%
        dplyr::reframe(timestamp = seq(from = ttimestamp,
                                       to = timestamp,
                                       length = 1 + ceiling((timestamp - ttimestamp) / maximum_gap_seconds))[-1],
                       speed_kmh = unique(speed_kmh),
                       interpolated = c(rep(T, length(timestamp) - 1), F),
                       time_travelled = rep(timestamp[2] - timestamp[1], length(timestamp)),
                       distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                       lon = tlon + (lon - tlon) * cumsum(time_travelled / sum(time_travelled, na.rm = T)),
                       lat = tlat + (lat - tlat) * cumsum(time_travelled / sum(time_travelled, na.rm = T))
        )

      interp <- to_interp %>%
        dplyr::select(!c("timestamp", "speed_kmh", "time_travelled", "distance_travelled", "lon", "lat")) %>%
        dplyr::left_join(interp, by = "id_ais_data_initial")

      out <- ais_data %>%
        dplyr::filter(!(id_ais_data_initial %in% unique(interp$id_ais_data_initial))) %>%
        dplyr::mutate(interpolated = F) %>%
        rbind(interp)

      rm(to_interp)
      rm(interp)

    } else {
      ais_ok <- ais_data %>%
        dplyr::filter(timestamp %in% timestamp_to_interpolate)

      ## select only timestamps where at least one vessel must be interpolated
      all_to_run <- na.omit(purrr::map_dbl(timestamp_to_interpolate, function(t) {
        done <- ais_ok$mmsi[ais_ok$timestamp %in% t]

        return(ifelse(all(ais_data$mmsi %in% done), NA, t))
      }))

      hour_to_run <- unique(lubridate::hour(lubridate::as_datetime(all_to_run)))

      cl <- parallel::makeCluster(nb_cores, outfile = outfile)
      doParallel::registerDoParallel(cl)

      ais_data <- purrr::map_dfr(hour_to_run, function(hh) {

        to_run <- all_to_run[hour(as_datetime(all_to_run)) == hh]

        datah <- data[data$timestamp %in% to_run, ] %>%
          dplyr::distinct()

        ais_datah <- ais_data %>%
          dplyr::filter(timestamp >= (min(to_run) - (5*60*60)) &
                          timestamp <= (max(to_run) + (5*60*60))) %>%
          dplyr::filter(X >= (min(datah$X, na.rm = T) - radius) & X <= (max(datah$X, na.rm = T) + radius) &
                          Y >= (min(datah$Y, na.rm = T) - radius) & Y <= (max(datah$Y, na.rm = T) + radius))

        out <- foreach::foreach(t = to_run,
                                .export = c("ais_ok", "all_to_run",
                                            "hh", "radius"),
                                .noexport = c("data", "ais_data"),
                                .packages = c("dplyr","tidyverse", "sf")
        ) %dopar% {
          cat(match(t, all_to_run), "/", length(all_to_run), "\n")

          data_coords <- datah[datah$timestamp == t, ]

          temp <- ais_datah %>%
            dplyr::filter(timestamp >= (t - 5*60*60) &
                            timestamp <= (t + 5*60*60)) %>%
            dplyr::filter(!(mmsi %in% unique(ais_ok$mmsi[ais_ok$timestamp %in% t]))) %>%
            dplyr::filter(X >= (min(data_coords$X, na.rm = T) - radius) & X <= (max(data_coords$X, na.rm = T) + radius) &
                            Y >= (min(data_coords$Y, na.rm = T) - radius) & Y <= (max(data_coords$Y, na.rm = T) + radius)) %>%
            dplyr::mutate(difftimestamp = timestamp - t)

          rm(data_coords)

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
                              tlat = prec$lat) %>%
                dplyr::filter(tmmsi == mmsi)

              rm(prec)

              interp_eez <- interp %>%
                dplyr::mutate(interpolated = T,
                              time_travelled = t - ttimestamp,
                              distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                              lon = tlon + (lon - tlon) * time_travelled / (timestamp - ttimestamp),
                              lat = tlat + (lat - tlat) * time_travelled / (timestamp - ttimestamp),
                              timestamp = t
                ) %>%
                dplyr::select(-c("tlon", "tlat", "tmmsi", "ttimestamp"))

              rm(to_interp)
              rm(interp)
              rm(interp_ref)

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

        gc()


        return(out)
      })

      parallel::stopCluster(cl)
      gc()

      out <- purrr::map_dfr(list(ais_ok,
                                 ais_data),
                            function(d) {return(d)}) %>%
        dplyr::filter(!duplicated(mmsi, timestamp)) %>%
        add_coordinates_meters(., crs_meters = crs_meters) %>%
        sf::st_drop_geometry()

      if (!("interpolated") %in% colnames(out)) {
        out <- out %>%
          dplyr::mutate(interpolated = F)
      }
    }

    ais_data <- AIStravel(ais_data = out %>%
                            dplyr::select(-c(time_travelled, distance_travelled, speed_kmh)),
                          crs_meters = crs_meters,
                          return_meter_coords = ifelse("X" %in% initial_columns & "Y" %in% initial_columns,
                                                      T,
                                                      F)) %>%
      dplyr::mutate(interpolated = ifelse(is.na(interpolated), F, interpolated))

    return(ais_data %>%
             dplyr::select(dplyr::all_of(c(initial_columns,
                                           colnames(ais_data)[!(colnames(ais_data) %in% initial_columns)]))))
  } else {

    cat("No point left in AIS data for interpolation.\n")

    if (!("X" %in% initial_columns) & !("Y" %in% initial_columns)) {
      ais_data <- ais_data %>%
        dplyr::select(-c(X, Y))
    }

    return(ais_data %>%
             dplyr::select(dplyr::all_of(c(initial_columns,
                                           colnames(ais_data)[!(colnames(ais_data) %in% initial_columns)]))))

  }

}
