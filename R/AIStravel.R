#### to estimate distance, time and speed between each AIS data per mmsi

#' AIStravel
#'
#' @param AOS AIS data. Must contain a column timestamp, lon, lat and mmsi (numeric value). the mmsi column is the identifier for vessel, and values can be replaced by the IMO for example, but the name of the column must be mmsi.
#' @param time_stop number of seconds that looked for interpolation of vessel positions. Interval of time higher than "time_stop" between 2 AIS receptions are considered as a stop of the movement. Filter also AIS data around data timestamp +- time_stop to accelerate the process.
#' @param mmsi_time_to_order if MMSI and time are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution.
#' @param return_sf if the output must be a sf object or a data frame with coordinates as column variable.
#' @param return_3035_coords if ETRS3035 coordinates must be returned as X and Y column variables in the output data.
#'
#' @return return the AIS data with the speed calculated based on the distance and time travelled since last AIS reception. Contains the columns:
#' time_travelled: number of seconds since the last reception of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessel since the last reception of an AIS signal.
#' X and Y columns if return_3035_coords = T (ETRS3035 coordinates)
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
