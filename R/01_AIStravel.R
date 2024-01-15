#' Estimate distance, time and speed (km/h) travelled by vessels
#'
#' Calculate the distance (meters), time (seconds) and speed (km/h) travelled by each MMSI (vessel) between each AIS reception.
#'
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param return_sf if TRUE, return an sf object. If FALSE a data frame with coordinates as column variable.
#' @param return_3035_coords if TRUE, return the ETRS3035 coordinates as X and Y columns in the output.
#'
#' @return return the AIS data with the distance, time travelled and the speed of the vessels since the last AIS reception. Contains the columns:
#' \itemize{
#' \item time_travelled: number of seconds since the last reception of an AIS signal (0 if first reception).
#' \item distance_travelled:  distance travelled (meters) since the last reception of an AIS signal (0 if first reception).
#' \item speed_kmh: speed (km/h) of the vessels since the last reception of an AIS signal.
#' \item X and Y columns (ETRS3035 coordinates) if return_3035_coords = T.
#' }
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' AIStravel(ais_data,
#'           time_stop = 5*60*60,
#'           mmsi_time_to_order = T,
#'           return_sf = F,
#'           return_3035_coords = F)}

AIStravel <- function(ais_data,
                      time_stop = 5*60*60,
                      mmsi_time_to_order = T,
                      return_sf = F,
                      return_3035_coords = F
) {
##
  if (!is.numeric(ais_data$timestamp)) {
    stop("timestamp must be numeric in ais_data")
  }
  if (!is.numeric(ais_data$lon)) {
    stop("lon must be numeric in ais_data")
  }
  if (!is.numeric(ais_data$lat)) {
    stop("lat must be numeric in ais_data")
  }
  if (!is.numeric(time_stop)) {
    stop("time_stop must be numeric")
  }
  if (!is.logical(return_sf)) {
    stop("return_sf must be numeric")
  }
  if (!is.logical(return_3035_coords)) {
    stop("return_3035_coords must be numeric")
  }

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
