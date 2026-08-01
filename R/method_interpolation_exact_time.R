#' Interpolate AIS positions
#'
#' Interpolates vessel positions at user-defined timestamps.
#' Interpolation can optionally be restricted to
#' a given radius within target locations
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`,
#'   `mmsi`, `X` and `Y` (`crs_meters` coordinates). `timestamp` must be Unix
#'   time (seconds since 1970-01-01), while
#'   `lon`, `lat`, `X` and `Y` must be numeric.
#' @param data target timestamps and locations. Contains `timestamp`,
#'   `lon`, `lat`, `X` and `Y` (`crs_meters` coordinates).
#' @param crs_meters CRS (in metres) used for distance calculations. Defaults
#'   to EPSG:3035.
#' @param radius Optional search radius (m) around each target location.
#' @param timestamp_to_interpolate Target timestamps.
#' @param nb_cores Number of CPU cores used.
#' @param outfile File used to save logs.
#'
#' @return The interpolated AIS data with an additional column:
#' \itemize{
#' \item `interpolated`: Whether the position was interpolated.
#' }
#' @keywords internal
#' @noRd
#'
method_interpolation_exact_time <- function(ais_data,
                                            data,
                                            crs_meters,
                                            radius,
                                            timestamp_to_interpolate,
                                            nb_cores = 1,
                                            outfile = "log.txt") {
   ais_ok <- ais_data %>%
     dplyr::filter(timestamp %in% timestamp_to_interpolate)

   ## select only timestamps where at least one vessel must be interpolated
   all_to_run <- stats::na.omit(purrr::map_dbl(timestamp_to_interpolate, function(t) {
     done <- ais_ok$mmsi[ais_ok$timestamp %in% t]

     return(ifelse(all(ais_data$mmsi %in% done), NA, t))
   }))

   assign_mmsi_to_core <- ais_data %>%
     dplyr::group_by(mmsi) %>%
     dplyr::summarise(n = dplyr::n()) %>%
     dplyr::ungroup() %>%
     dplyr::arrange(-n) %>%
     dplyr::mutate(core = rep(1:nb_cores, ceiling(dplyr::n() / nb_cores))[1:dplyr::n()])

   ais_data <- purrr::map(unique(assign_mmsi_to_core$core), function(co) {
     ais_data %>%
       dplyr::filter(mmsi %in% (assign_mmsi_to_core %>%
                                  dplyr::filter(core == co) %>%
                                  dplyr::pull(mmsi)))
   })

   cl <- parallel::makeCluster(nb_cores, outfile = outfile)
   doParallel::registerDoParallel(cl)

   ais_data <- foreach::foreach(ais_data_core = ais_data,
                           .noexport = c("assign_mmsi_to_core", "ais_data"),
                           .packages = c("dplyr", "purrr")
   ) %dopar% {

     purrr::map_dfr(all_to_run, function(t) {
       cat(match(t, all_to_run), "/", length(all_to_run), "\n")

       data_coords <- data[data$timestamp == t, ]

       temp <- ais_data_core %>%
         dplyr::filter(!(mmsi %in% unique(ais_ok$mmsi[ais_ok$timestamp %in% t]))) %>%
         dplyr::filter(X >= (min(data_coords$X, na.rm = T) - radius)) %>%
         dplyr::filter(X <= (max(data_coords$X, na.rm = T) + radius)) %>%
         dplyr::filter(Y >= (min(data_coords$Y, na.rm = T) - radius)) %>%
         dplyr::filter(Y <= (max(data_coords$Y, na.rm = T) + radius)) %>%
         dplyr::mutate(difftimestamp = timestamp - t)

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

           interp <- interp_ref %>%
             dplyr::mutate(ttimestamp = prec$timestamp,
                           tmmsi = prec$mmsi,
                           tlon = prec$lon,
                           tlat = prec$lat) %>%
             dplyr::filter(tmmsi == mmsi)

           interp_eez <- interp %>%
             dplyr::mutate(interpolated = T,
                           time_travelled = t - ttimestamp,
                           distance_travelled = 1000 * speed_kmh * (time_travelled / (60*60)),
                           lon = tlon + (lon - tlon) * time_travelled / (timestamp - ttimestamp),
                           lat = tlat + (lat - tlat) * time_travelled / (timestamp - ttimestamp),
                           timestamp = t
             ) %>%
             dplyr::select(-c("tlon", "tlat", "tmmsi", "ttimestamp"))

           out <- purrr::map_dfr(list(out_ok,
                                      interp_eez),
                                 function(d) {return(d)}) %>%
             dplyr::select(-"difftimestamp") %>%
             dplyr::mutate(timestamp = t)

           return(out)

         } else {
           return(out_ok %>%
                    dplyr::select(-"difftimestamp") %>%
                    dplyr::mutate(timestamp = t))
         }
       } else {
         return(to_interp %>%
                  dplyr::select(-"difftimestamp"))
       }
     })

   }

   parallel::stopCluster(cl)
   gc()

   out <- purrr::map_dfr(list(ais_ok,
                              ais_data),
                         function(d) {return(d)}) %>%
     add_coordinates_meters(., crs_meters = crs_meters) %>%
     sf::st_drop_geometry()

   if (!("interpolated") %in% colnames(out)) {
     out <- out %>%
       dplyr::mutate(interpolated = F)
   }

   return(out)
 }
