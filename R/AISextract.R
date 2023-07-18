################
## function to compute function extract_AIS_presence() in an optimized way
## need :
## data : data frame with points where AIS must be extracted. Must contain timestamp, lon, lat
## ais_data : ais data containing timestamp, lon, lat, mmsi
## All the other parameter for the extract_AIS_presence() function
## filter_radius_m: NA to not filter, or distance in m to return only lines with these distances

## advice : run this function in a for or lapply loop to apply it per day. The function can return a big dataset with combination of mmsi find for each
## data point, so doing this decrease the final dataset returned. The function apply the process for each day in any case.
## dont use colnames idd

## return only data when dates are into ais data ?


# source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-1_extend_data_time.R")
# source("C:/Users/234028/Documents/Gitlab/CoastalFutures/source/AIS/03-2_extract_AIS.R")

#' Extract vessels data at desired times in a desired radius
#'
#' @param data Your data frame, with a column timestamp, lon and lat (numeric value of time, longitude, latitude)
#' @param ais_data AIS data, with a column timestamp, lon, lat and mmsi (numeric value of time, longitude, latitude, Maritime Mobile Service Identity)
#' @param search_into_radius_m radius  in which vessels positions are returned
#' @param max_time_diff number of seconds before the data time, when boat positions are considered/extracted.
#' @param duplicate_time  if vessels positions must be extracted only for data times (TRUE) or every "t_gap" seconds up to max_time_diff before data times.
#' @param t_gap interval of time where vessels positions are extracted, from the data time to "max_time_diff" seconds before.
#' @param average_at number of seconds where times are averaged if accelerate = TRUE to decrease number of data time to extract. If average_at = 10, data times are averaged in the interval time-5:time+5.
#' @param accelerate TRUE or FALSE: if data times must be averaged within "average_at" seconds, to equlize times where vessels positions are extracted and decreased strongly computation time.
#'
#' @return to add
#' @export
#'
#' @examples to add
AISextract <- function(data,
                       ais_data,
                       # parallelize = T,
                       # core_to_use = NA,
                       search_into_radius_m = 20000,
                       max_time_diff = 30 * 60,
                       duplicate_time = F,
                       t_gap = 15,
                       average_at = 10,
                       # onLand = T,
                       accelerate = F
                       # limited_to_AIS_time = F
                       ) {

  # pack <- c("tidyverse", "dplyr", "sf", "lubridate")
  # inst <- which(!(pack %in% installed.packages()[,1]))
  #
  # if (length(inst) > 0) {
  #   lapply(pack[inst], function(p) {install.packages(p)})
  # }
  #
  # lapply(pack, library, character.only = TRUE)

  d_max <- (search_into_radius_m*1.5 + 2000) / (8*1e3)
  ## for 1 deg of difference, maximum distance of 111 km on the planet, minimum distance of 9.7

  ais_data_ref <- ais_data

  ais_data <- ais_data %>%
    dplyr::filter(timestamp >= (min(data$timestamp, na.rm = T) - (t_gap + max_time_diff + average_at)) &
                    timestamp <= (max(data$timestamp, na.rm = T) + t_gap + average_at)) %>%
    filter(lon > (min(data$lon) - d_max) & lon < (max(data$lon) + d_max) &
             lat > (min(data$lat) - d_max) & lat < (max(data$lat) + d_max))

  dates_ais <- as.character(unique(lubridate::date(lubridate::as_datetime(ais_data$timestamp))))

  data <- data %>%
    dplyr::mutate(date = as.character(lubridate::date(lubridate::as_datetime(timestamp))))

  colnam <- colnames(ais_data)
  if (any(colnam[!(colnam %in% c("timestamp", "lon", "lat", "mmsi"))] %in% c(colnames(data), "X", "Y"))) {
    colnames(ais_data)[colnam %in% c(colnames(data), "X", "Y") & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))] <- paste0("ais_", colnam[colnam %in% c(colnames(data), "X", "Y") & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))])
    cat(paste0("'", paste(colnam[colnam %in% c(colnames(data), "X", "Y") & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))], collapse = ", "), "'"), "columns in AIS data renamed as", paste0("'", paste(colnames(ais_data)[colnam %in% c(colnames(data), "X", "Y") & !(colnam %in% c("timestamp", "lon", "lat", "mmsi"))], collapse = ", "), "'"), "\n")
  }
  rm(colnam)

  if (any(colnames(data) == "mmsi")) {
    colnames(data)[colnames(data) == "mmsi"] <- "initial_mmsi"
    cat("mmsi column in dataframe renamed as 'initial_mmsi'")
  }

  eff_d <- map_dfr(dates_ais, function(d) {
    eff_temp <- data %>%
      filter(date == d) %>%
      # dplyr::mutate(id_point_effort = 1:n()) %>%
      arrange(timestamp)

    ## if duplicate time, duplicate the time until max_time_diff secondes before each point, by interval times of t_gap.
    ## if accelerate, do this but decrease the number of times to extract by assigning the times to the closest time step designed (each one separated by t_gap up to max_time_diff before)
    if (duplicate_time) {
      eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = max_time_diff, t_gap = t_gap, average_at = average_at)
      # } else if (accelerate) {
      #   eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap)
    } else {
      eff_temp <- data_extend_time(data = eff_temp, accelerate = accelerate, max_time_diff = 0, t_gap = t_gap, average_at = average_at)
    }
    ## so here : the real time of effort is the timestamp_ofEffort, while we extract AIS with max_time = 0 on timestamp = timestamp_AIS_to_extract
    eff_temp <- eff_temp %>%
      dplyr::mutate(timestamp_ofEffort = timestamp,
                    timestamp = timestamp_AIS_to_extract)

    return(eff_temp)
  })

  rm(data)

  eff_d <- eff_d %>%
    arrange(timestamp_AIS_to_extract) %>%
    dplyr::mutate(dayhour = paste(lubridate::date(datetime_AIS_to_extract), lubridate::hour(datetime_AIS_to_extract)))

  tot <- unique(eff_d$dayhour)

  # list_textract <- unique(eff_d$timestamp_AIS_to_extract)
  pb <- txtProgressBar(min = 0, max = length(tot), style = 3)

  daily_ais <- map_dfr(tot, function(h) {
    setTxtProgressBar(pb, match(h, tot))

    eff_h <- eff_d %>%
      filter(dayhour == h) %>%
      dplyr::mutate(tlon = lon, tlat = lat) %>%
      st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
      st_transform(crs = 3035)

    coords_eff_h <- st_coordinates(eff_h)

    eff_h <- eff_h %>%
      st_drop_geometry() %>%
      dplyr::mutate(X = coords_eff_h[,1],
                    Y = coords_eff_h[,2])

    rm(coords_eff_h)

    hourly_mmsi <- ais_data %>%
      filter(timestamp > (min(eff_h$timestamp, na.rm = T) - t_gap - average_at) &
               timestamp < (max(eff_h$timestamp, na.rm = T) + t_gap + average_at)) %>%
      filter(lon > (min(eff_h$lon) - d_max) & lon < (max(eff_h$lon) + d_max) &
               lat > (min(eff_h$lat) - d_max) & lat < (max(eff_h$lat) + d_max)) %>%
      dplyr::mutate(ais_lon = lon,
                    ais_lat = lat)

    if (nrow(hourly_mmsi) > 0) {
      # eff_h_spatiallimits <- eff_h %>%
      #   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
      #   st_transform(crs = 3035) %>%
      #   st_buffer(units::set_units(search_into_radius_m*1.5,m)) %>%
      #   group_by() %>%
      #   dplyr::summarise(do_union = T)

      hourly_mmsi <- hourly_mmsi %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs = 3035)

      coords_hourly_mmsi <- hourly_mmsi %>%
        st_coordinates()

      hourly_mmsi <- hourly_mmsi %>%
        st_drop_geometry() %>%
        dplyr::mutate(ais_X = coords_hourly_mmsi[,1],
                      ais_Y = coords_hourly_mmsi[,2])

      rm(coords_hourly_mmsi)

      # hourly_mmsi <- hourly_mmsi %>%
      #   dplyr::mutate(idd_ais_data = 1:n()) %>%
      #   group_by(idd_ais_data) %>%
      #   dplyr::mutate(dist_closer_eff = min(sqrt((ais_X - eff_h$X)^2 + (ais_Y - eff_h$Y)^2))) %>%
      #   ungroup() %>%
      #   filter(dist_closer_eff <= search_into_radius_m*1.2) %>%
      #   dplyr::select(-dist_closer_eff)
    }

    ais_on_effort <- data_extract_ais(data = eff_h,
                                          data_mmsi = hourly_mmsi,
                                          search_into_radius_m = search_into_radius_m,
                                          max_time_diff = 0,
                                          duplicate_time = F,
                                          t_gap = t_gap,
                                          average_at = 0,
                                          # onLand = onLand,
                                          # list_t_extract = list_textract,
                                          accelerate = F
                                          # limited_to_AIS_time = limited_to_AIS_time
                                          )

    ais_on_effort <- ais_on_effort %>%
      dplyr::mutate(inside_daily_AIS_time_range = ifelse(timestamp >= (min(ais_data_ref$timestamp, na.rm = T) - t_gap - average_at) & timestamp <= (max(ais_data_ref$timestamp, na.rm = T) + t_gap + average_at),
                                                         T, F))

    rm(eff_h)
    rm(hourly_mmsi)

    return(ais_on_effort %>%
             dplyr::mutate(timestamp = timestamp_ofEffort) %>%
             dplyr::select(!c("timestamp_ofEffort", "dayhour")))

  })

  rm(d_max)
  rm(ais_data)
  rm(eff_d)
  rm(dates_ais)

  return(daily_ais)

}
