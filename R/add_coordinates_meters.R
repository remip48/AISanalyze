#' Add spatial coordinates in the crs in meters
#'
#' @param data data where coordinates are added
#' @param crs_meters target crs (must have unit in meters)
#' @param coordinates_to_write coordinates to have in the final dataset
#'
#' @return same dataset with added coordinates
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' ais <- add_coordinates_meters(ais, crs_meters = 3035, coordinates_to_write = c("X", "Y"))}
#'
add_coordinates_meters <- function(data, crs_meters = 3035, coordinates_to_write = c("X", "Y")) {
  if (!(all(coordinates_to_write %in% colnames(data)))) {
    if (!("sf" %in% class(data))) {
      data <- data %>%
        dplyr::mutate(tlon = lon,
                      tlat = lat) %>%
        st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }

    data <- data %>%
      st_transform(crs = crs_meters)

    coords_eff <- data %>%
      st_coordinates() %>%
      as.data.frame()

    data <- data %>%
      dplyr::mutate(X = coords_eff[,1],
                    Y = coords_eff[,2]) %>%
      st_cast()

    colnames(data)[colnames(data) == "X"] <- coordinates_to_write[1]
    colnames(data)[colnames(data) == "Y"] <- coordinates_to_write[2]

    rm(coords_eff)
  }

  return(data)
}
