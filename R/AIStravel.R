#### to estimate distance, time and speed between each AIS data per mmsi
## need :
## lon, lat, timestamp, mmsi columns
## AIS as parameter : AIS data
## time_stop : time interval without AIS data for a single mmsi, in second, after what the first point is define as a new start (speed, distance and time = 0)
## meaning possible pause during ship track or to limitate uncertainties of AIS interpolation. By default, no time_stop

#' Title
#'
#' @param AIS AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity).
#' @param time_stop time defining the maximum time interval where a vessel position is taken into account for analyze. If more, distance, speed and time for first point is define as 0.
#' @param mmsi_time_to_order if mmsi & time column must be ordered, if FALSE, is already ordered by MMSI and then by timestamp (arrange(mmsi, timestamp)).
#'
#' @return to add
#' @importFrom magrittr %>%
#' @export
#'
#' @examples # to add
AIStravel <- function(AIS,
                      time_stop = Inf,
                      mmsi_time_to_order = T,
                      return_sf = F,
                      return_3035_coords = F
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

  if (mmsi_time_to_order) {
    AIS <- AIS %>%
      dplyr::arrange(mmsi, timestamp)
  }

  AIS <- AIS %>%
    dplyr::filter(!duplicated(paste0(mmsi, timestamp)))

  AIS <- AIS %>%
    dplyr::mutate(tlon = lon,
           tlat = lat) %>%
    st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
    st_transform(crs = 3035)

  coords_AIS <- AIS %>%
    st_coordinates()

  mmsi_prev <- AIS$mmsi[-nrow(AIS)]

  if (!return_sf) {
    AIS <- AIS %>%
      st_drop_geometry()
  }

  AIS <- AIS %>%
    dplyr::mutate(X = coords_AIS[,1],
                  Y = coords_AIS[,2],
                  tmmsi = c("initial", mmsi_prev)) %>%
    filter(!is.na(X) & !is.na(Y) & !is.nan(X) & !is.nan(Y))

  AIS <- AIS %>%
    dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
                  time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                  distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                  speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
    ) %>%
    dplyr::select(-c("tmmsi"))

  if (!return_3035_coords) {
    AIS <- AIS %>%
      dplyr::select(-c("X", "Y"))
  }

  rm(mmsi_prev)
  rm(coords_AIS)

  return(AIS)

}
