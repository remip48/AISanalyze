#' to estimate distance, time and speed between each AIS data per mmsi
#'
#' @param AIS AIS data. Must contain a column timestamp, lon, lat and mmsi (numeric value). the mmsi column is the identifier for vessel, and values can be replaced by the IMO for example, but the name of the column must be mmsi.
#' @param time_stop number of seconds around the AIS reception considered for interpolation of vessel positions. Interval of time higher than "time_stop" between 2 AIS receptions are considered as a stop of the movement. Filter also AIS data around data timestamp +- time_stop to accelerate the process.
#' @param mmsi_time_to_order if MMSI and time are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution.
#' @param return_sf if the output is a sf object or a data frame with coordinates as column variable.
#' @param return_3035_coords if ETRS3035 coordinates are returned as X and Y column variables in the output data.
#'
#' @return return the AIS data with the speed calculated based on the distance and time travelled since last AIS reception. Contains the columns:
#' time_travelled: number of seconds since the last reception of an AIS signal (0 if first reception).
#' distance_travelled:  distance travelled (meters) since the last reception of an AIS signal (0 if first reception).
#' speed_kmh: speed (km/h) of the vessel since the last reception of an AIS signal.
#' X and Y columns if return_3035_coords = T (ETRS3035 coordinates)
#' @export
#'
#' @examples # to add

AIStravel <- function(ais_data,
                      time_stop = 5*60*60,
                      mmsi_time_to_order = T,
                      return_sf = F,
                      return_3035_coords = F
) {

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate", "units", "purrr", "stats", "utils", "stringr", "doParallel")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)
  }

  ais_data <- ais_data[!duplicated(paste0(ais_data$mmsi, ais_data$timestamp)), ]

  ####
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

    if (!return_sf) {
      ais_data <- ais_data %>%
        sf::st_drop_geometry()
    }

    ais_data <- ais_data %>%
      # sf::st_drop_geometry() %>%
      dplyr::mutate(X = coords_AIS[,1],
                    Y = coords_AIS[,2])

    rm(coords_AIS)
  }

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]
  ####

  # ais_data <- ais_data %>%
  #   dplyr::mutate(tlon = lon,
  #                 tlat = lat) %>%
  #   sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
  #   sf::st_transform(crs = 3035)
#
  # coords_ais_data <- ais_data %>%
  #   st_coordinates()

  mmsi_prev <- ais_data$mmsi[-nrow(ais_data)]

  # if (!return_sf) {
  #   ais_data <- ais_data %>%
  #     sf::st_drop_geometry()
  # }

  ais_data <- ais_data %>%
    dplyr::mutate(
      # X = coords_ais_data[,1],
      #             Y = coords_ais_data[,2],
                  tmmsi = c("initial", mmsi_prev)) #%>%
    # dplyr::filter(!is.na(X) & !is.na(Y) & !is.nan(X) & !is.nan(Y))

  ais_data <- ais_data %>%
    dplyr::mutate(time_travelled = timestamp - c(first(timestamp), timestamp[-n()]),
                  time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                  distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                  speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
    ) %>%
    dplyr::select(-c("tmmsi"))

  if (!return_3035_coords) {
    ais_data <- ais_data %>%
      dplyr::select(-c("X", "Y"))
  }

  rm(mmsi_prev)
  # rm(coords_ais_data)

  return(ais_data)

}
