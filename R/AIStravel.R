#### to estimate distance, time and speed between each AIS data per mmsi
## need :
## lon, lat, timestamp, mmsi columns
## AIS as parameter : AIS data
## time_stop : time interval without AIS data for a single mmsi, in second, after what the first point is define as a new start (speed, distance and time = 0)
## meaning possible pause during ship track or to limitate uncertainties of AIS interpolation. By default, no time_stop

#' Title
#'
#' @param AIS AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity).
#' @param time_stop time defining the maximum time interval where a vessel position is interpolated.
#'
#' @return to add
#' @export
#'
#' @examples to add
AIStravel <- function(AIS,
                      time_stop = Inf
                      # parallelize=T,
                      # core_to_use=NA
) {

  # load_packages <- function(p) {
  #   if (p %in% rownames(installed.packages())) {
  #     library(p, character.only = TRUE)
  #   } else {
  #     install.packages(p)
  #     library(p, character.only = TRUE)
  #   }
  # }
  # lapply(c("tidyverse", "dplyr", "sf"), load_packages)

  AIS <- AIS %>%
    arrange(mmsi, timestamp) %>%
    mutate(tlon = lon,
           tlat = lat
    ) %>%
    st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
    st_transform(crs = 3035)

  coords_AIS <- AIS %>%
    st_coordinates()

  AIS <- AIS %>%
    st_drop_geometry() %>%
    mutate(X = coords_AIS[,1],
           Y = coords_AIS[,2]) %>%
    group_by(mmsi) %>%
    mutate(distance_travelled = c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2)),
           time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
           speed_kmh = c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1]))
    ) %>%
    ungroup() %>%
    mutate(speed_kmh = ifelse(time_travelled > time_stop, 0, speed_kmh),
           distance_travelled = ifelse(time_travelled > time_stop, 0, distance_travelled),
           time_travelled = ifelse(time_travelled > time_stop, 0, time_travelled)) %>%
    dplyr::select(-c("X", "Y"))

  return(AIS)

}
