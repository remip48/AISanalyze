#' Interpolate AIS positions
#'
#' Interpolate AIS positions at regular time intervals
#'
#' @param ais_data AIS data frame containing `timestamp`, `lon`, `lat`, and
#'   `mmsi`. `timestamp` must be Unix time (seconds since 1970-01-01), while
#'   `lon` and `lat` must be numeric.
#' @param maximum_gap_seconds threshold of time interval between AIS positions
#' (seconds) above which the track is interpolated.
#'
#' @return The interpolated AIS data with an additional column:
#' \itemize{
#' \item `interpolated`: Whether the position was interpolated.
#' }
#' @keywords internal
#' @noRd
#'
method_interpolation_max_time <- function(ais_data,
                                          maximum_gap_seconds) {
  ais_data <- ais_data %>%
    dplyr::mutate(id_ais_data_initial = 1:dplyr::n())

  to_interp <- ais_data %>%
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(idd = 1:dplyr::n()) %>%
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

  return(ais_data %>%
           dplyr::filter(!(id_ais_data_initial %in% unique(interp$id_ais_data_initial))) %>%
           dplyr::mutate(interpolated = F) %>%
           rbind(interp))
}
