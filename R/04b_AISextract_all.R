#' Extract all AIS positions in the desired interval of time
#'
#' @param data Data of interest for the extraction of AIS. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude) & lat (latitude). timestamp, lon and lat must be numeric.
#' @param crs_meters projection (crs) in 'meters' to use to calculate distance over the study area. Default to 3035 (ETRS89).
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param search_into_radius_m radius (kilometer) where the MMSIs are extracted and returned.
#' @param interval_time_before Number of seconds before each time from "data" where AIS data are returned. Example: 60 -> all AIS data from 60 seconds before data "time" to the data "time" are returned (calculated for each line of data independantly).
#' @param interval_time_after Number of seconds after each time from "data" where AIS data are returned. Example: 60 -> all AIS data from data "time" until 60 seconds after are returned (calculated for each line of data independantly).
#'
#' @return return the input data with the AIS extracted merged in the dataframe: each line of input data is duplicated by timestamp to extract (every "t_gap" number of seconds up to "max_time_diff" number of seconds). All these lines are duplicated for each MMSI present in the area at the moment of the extraction. If no AIS are present in the radius at this moment, the columns dedicated to AIS data are filled with NA, so that no input data and no timestamp to extract is lost.
#' The output dataframe contains the columns of the input data, the columns of the AIS data (with "ais_" as prefix if the same column is already present in the input data), and the following columns:
#' \itemize{
#' \item distance_effort_ais_m: distance (meters) between the data location and the MMSI at this time (filled with NA if no MMSI).
#' \item timestamp_AIS_to_extract: timestamp for the extraction of the AIS (approximated with "average_at" number of seconds if accelerate = TRUE).
#' \item diffTime_AIS_extraction_effort: difference (in seconds) between the timestamp to extract (timestamp_AIS_to_extract) and the real data timestamp.
#' \item datetime_AIS_to_extract: datetime of timestamp_AIS_to_extract.
#' \item diffTime_AIS_effort: difference, in seconds, between the AIS data and the data timestamp: can be different from diffTime_AIS_extraction_effort due to the parameter "average_at" (that average the timestamp to extract).
#'
#' @examples
#' \dontrun{
#' data("ais")
#' data("point_to_extract")
#'
#' library(dplyr)
#' library(lubridate)
#' point_to_extract <- point_to_extract %>%
#'   mutate(timestamp = as.numeric(ymd_hm(datetime)))
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             time_stop = 5*3600,
#'             mmsi_time_to_order = T,
#'             return_sf = F,
#'             return_3035_coords = F) %>%
#'   AISinterpolate_at(data = point_to_extract,
#'                   ais_data = .,
#'                   mmsi_time_to_order = T,
#'                   load_existing_files = T,
#'                   save_AISinterlate_at = T,
#'                   overwrite = F,
#'                   file_AISinterlate_at = "AISinterpolate_at",
#'                   radius = 200000,
#'                   time_stop = 5*3600,
#'                   correct_speed = T,
#'                   threshold_speed_to_correct = 100,
#'                   filter_station = T,
#'                   filter_high_speed = T,
#'                   parallelize = F)
#'
#' AISextract_all(data = point_to_extract,
#'            ais_data = ais,
#'            search_into_radius_m = 50000,
#'            interval_time_before = 5 * 60,
#'            interval_time_after = 5 * 60)}
#' @export

AISextract_all <- function(data,
                           crs_meters = 3035,
                           ais_data,
                           search_into_radius_m = 50000,
                           interval_time_before = 5 * 60,
                           interval_time_after = 5 *	60)
{
  ais_data <- ais_data[ais_data$timestamp >= (min(data$timestamp,
                                                  na.rm = T) - (interval_time_before)) & ais_data$timestamp <=
                         (max(data$timestamp, na.rm = T) + interval_time_after),
  ]
  if (!(all(c("ais_X", "ais_Y") %in% colnames(ais_data)))) {
    if (!(all(c("X", "Y") %in% colnames(ais_data)))) {
      if (!("sf" %in% class(ais_data))) {
        ais_data <- ais_data %>% dplyr::mutate(tlon = lon,
                                               tlat = lat) %>% st_as_sf(coords = c("tlon",
                                                                                   "tlat"), crs = 4326)
      }
      ais_data <- ais_data %>% st_transform(crs = crs_meters)
      coords_eff <- ais_data %>% st_coordinates() %>% as.data.frame()
      ais_data <- ais_data %>% st_drop_geometry() %>% dplyr::mutate(X = coords_eff[,
                                                                                   1], Y = coords_eff[, 2]) %>% dplyr::rename(ais_X = X,
                                                                                                                              ais_Y = Y)
      rm(coords_eff)
    }
    else {
      ais_data <- ais_data %>% dplyr::rename(ais_X = X,
                                             ais_Y = Y)
    }
  }
  colnam <- colnames(ais_data)
  if (any(colnam[!(colnam %in% c("timestamp", "mmsi"))] %in%
          c(colnames(data)))) {
    colnames(ais_data)[colnam %in% c(colnames(data)) & !(colnam %in%
                                                           c("timestamp", "mmsi"))] <- paste0("ais_", colnam[colnam %in%
                                                                                                               c(colnames(data)) & !(colnam %in% c("timestamp",
                                                                                                                                                   "mmsi"))])
    cat("\n", paste0("'", paste(colnam[colnam %in% colnames(data) &
                                         !(colnam %in% c("timestamp", "mmsi"))], collapse = ", "),
                     "'"), "columns in AIS data renamed as", paste0("'",
                                                                    paste(colnames(ais_data)[colnam %in% colnames(data) &
                                                                                               !(colnam %in% c("timestamp", "mmsi"))], collapse = ", "),
                                                                    "'"), "\n")
  }
  rm(colnam)
  if (any(colnames(data) == "mmsi")) {
    colnames(data)[colnames(data) == "mmsi"] <- "initial_mmsi"
    cat("\nmmsi column in dataframe renamed as 'initial_mmsi'")
  }
  if (!(all(c("X", "Y") %in% colnames(data)))) {
    if (!("sf" %in% class(data))) {
      data <- data %>% dplyr::mutate(tlon = lon, tlat = lat) %>%
        st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }
    data <- data %>% st_transform(crs = crs_meters)
    coords_eff <- data %>% st_coordinates() %>% as.data.frame()
    data <- data %>% st_drop_geometry() %>% dplyr::mutate(X = coords_eff[,
                                                                         1], Y = coords_eff[, 2])
    rm(coords_eff)
  }
  data <- data %>% dplyr::mutate(idd_effort = 1:n())
  return_columns <- function(.x) {
    return(.x)
  }
  ais_data <- ais_data %>% as.data.frame() %>% dplyr::rename(ais_timestamp = timestamp)
  time_ais <- map_dfr(unique(data$timestamp), function(dt) {
    eff_dt <- data[data$timestamp == dt, ]
    mmsi_ref <- ais_data[ais_data$ais_timestamp >= (dt -
                                                      interval_time_before) & ais_data$ais_timestamp <=
                           (dt + interval_time_after) & ais_data$ais_X >= (min(eff_dt$X) -
                                                                             search_into_radius_m) & ais_data$ais_X <= (max(eff_dt$X) +
                                                                                                                          search_into_radius_m) & ais_data$ais_Y >= (min(eff_dt$Y) -
                                                                                                                                                                       search_into_radius_m) & ais_data$ais_Y <= (max(eff_dt$Y) +
                                                                                                                                                                                                                    search_into_radius_m), ]
    if (nrow(mmsi_ref) > 1) {
      out <- eff_dt %>% as.data.frame() %>% group_by(idd_effort) %>%
        dplyr::reframe(across(colnames(eff_dt)[colnames(eff_dt) !=
                                                 "idd_effort"], ~return_columns(.x)), mmsi_ref %>%
                         dplyr::mutate(distance_effort_ais_m = sqrt((ais_X -
                                                                       X)^2 + (ais_Y - Y)^2))) %>% dplyr::filter(distance_effort_ais_m <=
                                                                                                                   search_into_radius_m)
    }
    else {
      out <- eff_dt
    }
    rm(eff_dt)
    rm(mmsi_ref)
    return(out)
  })
  if (!("ais_timestamp" %in% colnames(time_ais))) {
    cat("\nNo AIS data for data processed between", as.character(lubridate::as_datetime(min(time_ais$timestamp))),
        "and", as.character(lubridate::as_datetime(max(time_ais$timestamp))),
        "\n")
    time_ais <- time_ais %>% dplyr::mutate(ais_timestamp = NA)
  }
  time_ais <- time_ais %>% dplyr::mutate(diffTime_AIS_effort = ais_timestamp -
                                           timestamp) %>% dplyr::select(-c("point")[c("point") %in%
                                                                                      colnames(time_ais)])
  if (any(!(data$idd_effort %in% time_ais$idd_effort))) {
    time_ais <- map_dfr(list(time_ais, data[!(data$idd_effort %in%
                                                time_ais$idd_effort), ]), function(l) {
                                                  return(l)
                                                })
  }
  gc()
  time_ais <- time_ais %>% dplyr::select(!c("idd_effort"))
  rm(data)
  return(time_ais)
}
