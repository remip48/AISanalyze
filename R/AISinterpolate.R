#' Interpolate AIS positions
#'
#' Interpolates vessel positions either: (depending on `type_interpolation`)
#'   \itemize{
#'   \item to ensure time intervals do not exceed a specified maximum
#'   (`maximum_time_interval`).
#'   \item at user-defined timestamps (`exact_timestamp`). Interpolation
#'   can optionally be restricted to a given radius within
#'   target locations to reduce computation time.
#'   }
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
#'   \item `locations_of_interest`: Optional data frame with `lon` and `lat`
#'     columns corresponding to each `timestamp_to_interpolate`.
#'   \item `radius`: Optional search radius (m) around each target location.
#'   }
#' @param crs_meters CRS (in metres) used for distance calculations. Defaults
#'   to EPSG:3035.
#' @param nb_cores Number of CPU cores used if `type_interpolation = exact_timestamp`.
#' @param outfile File used to save logs when `type_interpolation = exact_timestamp`.
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
#'   AIStravel(ais_data = .)
#'
#' # to interpolate all vessel locations separated by > 60 seconds
#' out <- AISinterpolate(ais_data = ais,
#'                type_interpolation = "maximum_time_interval",
#'                maximum_time_interval = list(maximum_gap_seconds = 60),
#'                crs_meters = 3035)
#'
#' # to interpolate all vessel locations at exact timestamps,
#' # within a radius of 200 000 meters around
#' # target locations
#' out <- AISinterpolate(ais_data = ais,
#'            type_interpolation = "exact_timestamp",
#'            exact_timestamp = list(
#'                timestamp_to_interpolate = point_to_extract$timestamp,
#'                locations_of_interest = data.frame(lon = point_to_extract$lon,
#'                                                  lat = point_to_extract$lat),
#'                radius = 200000),
#'            crs_meters = 3035)
#'            }
#' @export

AISinterpolate <- function(ais_data,
                              type_interpolation,
                              maximum_time_interval = list(maximum_gap_seconds = 10 * 60),
                              exact_timestamp = list(timestamp_to_interpolate = NULL,
                                                     locations_of_interest = NULL,
                                                     radius = 200000),
                              crs_meters = 3035,
                              nb_cores = 1,
                              outfile = "log.txt"
){

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    stop("Please run AIStravel() before AISinterpolate()")
  }

  ## set up the parameters of interpolation according to the type of interpolation
  if (type_interpolation == "exact_timestamp") {

    locations_of_interest <- exact_timestamp$locations_of_interest
    timestamp_to_interpolate <- exact_timestamp$timestamp_to_interpolate
    radius <- exact_timestamp$radius

    if (all(is.null(locations_of_interest))) {
      cat("All AIS data will be interpolated at `timestamp_to_interpolate`. If you want to target a specific region, use `locations_of_interest` argument.")
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

  initial_columns <- colnames(ais_data)

  data <- add_coordinates_meters(data, crs_meters = crs_meters) %>%
    sf::st_drop_geometry()

  ais_data <- ais_data %>%
    add_coordinates_meters(., crs_meters = crs_meters) %>%
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
      out <- method_interpolation_max_time(ais_data,
                                           maximum_gap_seconds)
    } else {
      out <- method_interpolation_exact_time(ais_data,
                                             data,
                                             crs_meters,
                                             radius,
                                             timestamp_to_interpolate,
                                             nb_cores,
                                             outfile)
    }

    ais_data <- AIStravel(ais_data = out %>%
                            dplyr::select(-c(time_travelled, distance_travelled, speed_kmh)),
                          crs_meters = crs_meters) %>%
      dplyr::mutate(interpolated = ifelse(is.na(interpolated), F, interpolated))

  } else {
    cat("No point left in AIS data for interpolation.\n")
  }

  return(ais_data %>%
           dplyr::select(dplyr::all_of(c(initial_columns, colnames(ais_data)[!(colnames(ais_data) %in% initial_columns)]))))
}
