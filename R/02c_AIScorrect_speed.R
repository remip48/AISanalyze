#' Correct GPS errors and delays from AIS data
#'
#' Correct the errors due to GPS delays or errors and leading to erroneous speeds (km/h), distance or time travelled by MMSI (vessel). The corrections are made according to different input parameters. Other important parameters are used in the function to detect GPS errors, and are not in the parameters of the function for simplicity. However these can be added on request for more flexibility in the filters
#'
#' @param ais_data AIS data. Must contain a column: timestamp (number of seconds since January 1, 1970 (the Unix epoch): see https://r-lang.com/how-to-convert-date-to-numeric-format-in-r/ for transformation), and the columns lon (longitude), lat (latitude) and mmsi (Maritime mobile service identity). timestamp, lon and lat must be numeric. The mmsi column is the identifier for the vessels, the values can be replaced by the IMO or another identifier, but the name of the column must be mmsi.
#' @param crs_meters projection (crs) in 'meters' to use to calculate distance over the study area. Default to 3035 (ETRS89).
#' @param mmsi_time_to_order if MMSI and timestamps are not yet arranged as dplyr::arrange(AIS data, mmsi, timestamp), must be TRUE. We recommand to put it as TRUE by precaution. Important to prevent large errors.
#' @param correct_high_speed_craft if speeds from high speed craft (specially used for aircraft) must be corrected as well.
#' @param threshold_speed_to_correct speeds higher than this threshold are corrected if the mmsi is not an aircraft and if correct_speed = T
#' @param threshold_speed_to_correct_expr expression (function having "speed_kmh" as unique parameter) to determine another threshold correcting GPS errors and delays. This expression is ran for each MMSI individually, allowing to identify unrealistic speeds based on the mean of the vessel speed, median, standard deviation or other functions. The default expression has been tested as relevant and appropriate to filter GPS errors and delays, still checks are necessary.
#' @param time_stop number of seconds before and after the AIS signal were the vessel track is not calculated/interpolated anymore if there is not another AIS signal meanwhile. Filter also AIS data too long before and after that are not of interest, to accelerate a lot the process.
#' @param nb_iteration number of iteration re-estimating the GPS errors. Low number (10 as default) is prefered in case GPS error is inherent and can not be deleted: in this case, the script will continue to delete points until nb_iteration is reached.
#' @param nb_iteration_successive_strange number of iterations where GPS delays are looked backwards. 100 as default. Is the number of possible subsequent GPS lag.
#'
#' @return Return the AIS data with identified GPS error or GPS delay removed. The vessel speeds are corrected, as well as the distance and time travelled. Contains columns:
#' \itemize{
#' \item speed_kmh_corrected: if TRUE, the speed of the line has been corrected.
#' \item time_travelled: number of seconds since the last reception or interpolation of an AIS signal (0 if first reception).
#' \item distance_travelled: distance travelled (meters) since the last reception or interpolation of an AIS signal (0 if first reception).
#' \item speed_kmh: speed (km/h) of the vessels since the last reception or interpolation of an AIS signal.
#' \item station: if the MMSI is a station or not.
#' \item high_speed: if the MMSI is a high speed craft (used for aircraft) or not.}
#'
#' @examples
#' \dontrun{
#' data("ais")
#'
#' library(dplyr)
#' library(lubridate)
#' ais <- ais %>%
#'   mutate(timestamp = as.numeric(ymd_hms(datetime))) %>%
#'   AIStravel(ais_data = .,
#'             time_stop = 5*60*60,
#'             mmsi_time_to_order = T,
#'             return_sf = F,
#'             return_3035_coords = F)
#'
#' AIScorrect_speed(ais_data = ais,
#'                  mmsi_time_to_order = T,
#'                  threshold_speed_to_correct = 100,
#'                  time_stop = 5*60*60)}
#' @export

AIScorrect_speed <- function(ais_data,
                             crs_meters = 3035,
                             mmsi_time_to_order = T,
                             threshold_speed_to_correct = 100,
                             threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 1], na.rm = T) +
                                                                                              sd(speed_kmh[speed_kmh > 1 & speed_kmh < quantile(speed_kmh[speed_kmh > 1], .75)]) * 5 + 15))},
                             time_stop = 5*60*60,
                             nb_iteration = 10,
                             nb_iteration_successive_strange = 100,
                             correct_high_speed_craft = F,
                             quantile_station = 0.975,
                             threshold_distance_station = 1,
                             quantile_high_speed = 0.97,
                             threshold_high_speed = 110
) {

  list_num <- c("mmsi_time_to_order", "correct_high_speed_craft")
  if (any(!do.call("c", map(list(mmsi_time_to_order, correct_high_speed_craft),
                            is.logical)))) {
    stop(paste0(paste(list_num[which(!do.call("c", map(list(mmsi_time_to_order, correct_high_speed_craft),
                                                       is.logical)))],
                      collapse = ", "),
                " must be logical"))
  }
  rm(list_num)

  list_num <- c("ais_data$timestamp",
                ifelse(all(c("X", "Y") %in% colnames(ais_data)), "ais_data$X", "ais_data$lon"),
                ifelse(all(c("X", "Y") %in% colnames(ais_data)), "ais_data$Y", "ais_data$lat"),
                "time_stop", "threshold_speed_to_correct", "quantile_station", "threshold_distance_station", "quantile_high_speed", "threshold_high_speed",
                "nb_iteration", "nb_iteration_successive_strange")
  if (any(!do.call("c", map(list(ais_data$timestamp,
                                 ifelse(all(c("X", "Y") %in% colnames(ais_data)), ais_data$X, ais_data$lon),
                                 ifelse(all(c("X", "Y") %in% colnames(ais_data)), ais_data$Y, ais_data$lat),
                                 time_stop, threshold_speed_to_correct, quantile_station, threshold_distance_station, quantile_high_speed, threshold_high_speed,
                                 nb_iteration, nb_iteration_successive_strange),
                            is.numeric)))) {
    stop(paste0(paste(list_num[which(!do.call("c", map(list(ais_data$timestamp,
                                                            ifelse(all(c("X", "Y") %in% colnames(ais_data)), ais_data$X, ais_data$lon),
                                                            ifelse(all(c("X", "Y") %in% colnames(ais_data)), ais_data$Y, ais_data$lat),
                                                            time_stop, threshold_speed_to_correct, quantile_station, threshold_distance_station, quantile_high_speed, threshold_high_speed,
                                                            nb_iteration, nb_iteration_successive_strange),
                                                       is.numeric)))],
                      collapse = ", "),
                " must be numeric"))
  }
  rm(list_num)

  init_cols <- colnames(ais_data)

  if (!(all(c("X", "Y") %in% colnames(ais_data)))) {
    if (!("sf" %in% class(ais_data))) {
      ais_data <- ais_data %>%
        dplyr::mutate(tlon = lon,
                      tlat = lat) %>%
        sf::st_as_sf(coords = c("tlon", "tlat"), crs = 4326)
    }
    # if (st_crs(ais_data)$input != "EPSG:3035") {
      ais_data <- ais_data %>%
        sf::st_transform(crs = crs_meters)
    # }

    coords_AIS <- ais_data %>%
      sf::st_coordinates() %>%
      as.data.frame()

    ais_data <- ais_data %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(X = coords_AIS[,1],
                    Y = coords_AIS[,2])

    rm(coords_AIS)
  }

  ais_data <- ais_data[!is.na(ais_data$X) & !is.na(ais_data$Y) & !is.nan(ais_data$X) & !is.nan(ais_data$Y), ]

  if (mmsi_time_to_order) {
    ais_data <- ais_data %>%
      dplyr::arrange(mmsi, timestamp)

    if (!("id_ais_data_initial" %in% colnames(ais_data))) {
      cat("Rewriting id_ais_data_initial\n")
    }
    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if (!("time_travelled" %in% colnames(ais_data)) | !("distance_travelled" %in% colnames(ais_data)) | !("speed_kmh" %in% colnames(ais_data))) {
    ais_data <- AIStravel(ais_data = ais_data,
                          crs_meters = crs_meters,
                          time_stop = time_stop,
                          mmsi_time_to_order = F,
                          return_sf = F,
                          return_3035_coords = T)
  }

  if (!("id_ais_data_initial" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if (!("id_mmsi_point_initial" %in% colnames(ais_data)) | !("station" %in% colnames(ais_data)) | !("high_speed" %in% colnames(ais_data))) {
    ais_data <- ais_data %>%
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(station = ifelse(quantile(distance_travelled, quantile_station, na.rm = T) <= threshold_distance_station, T, F),
                    high_speed = ifelse(quantile(speed_kmh, quantile_high_speed, na.rm = T) >= threshold_high_speed, T, F),
                    id_mmsi_point_initial = 1:n()) %>%
      ungroup()
  }

  if (correct_high_speed_craft) {
    ais_data <- ais_data %>%
      dplyr::mutate(real_high_speed = high_speed,
                    high_speed = F)
  }

  # ais_data = ais_data_ref %>%
  #   filter(mmsi == 265719460) %>%
  #   # filter(id_mmsi_point_initial %in% 18700:19000) %>%
  #   mutate(id_mmsi_point_initial = 1:n())


  # threshold_speed_to_correct_expr = function(speed_kmh) {return((median(speed_kmh[speed_kmh > 1], na.rm = T) +
  #                                                                  sd(speed_kmh[speed_kmh > 1 & speed_kmh < quantile(speed_kmh[speed_kmh > 1], .75)]) * 5 + 15))}

  strange_speed <- ais_data %>%  # [(ais_data$speed_kmh > threshold_speed_to_correct | ais_data$speed_kmh >) &
    dplyr::group_by(mmsi) %>%
    dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr(speed_kmh)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(id_mmsi_point_initial != 1) %>%
    dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                   threshold_speed_to_correct,
                                                   threshold_strange_speed)) %>%
    dplyr::filter((speed_kmh > threshold_speed_to_correct |
                     speed_kmh > threshold_strange_speed) &
                    (time_travelled < time_stop &
                       !(high_speed) &
                       !(station)))

  # thresh <- unique(strange_speed$threshold_strange_speed)

  # strange_speed$threshold_strange_speed

  strange_speed <- strange_speed[c(T, (strange_speed$id_ais_data_initial[-1] - strange_speed$id_ais_data_initial[-nrow(strange_speed)]) > 2), ]

  strange_speed <- strange_speed$id_ais_data_initial

  # ggplot() +
  #   geom_point(data = ais_data_ref %>%
  #                filter(mmsi == 265719460) %>%
  #                # filter(id_mmsi_point_initial %in% 18700:19000) %>%
  #                mutate(id_mmsi_point_initial = 1:n()), aes( x = lon, y = lat), color = "orange") +
  #   geom_point(data = ais_data, aes(x = lon, y = lat, color = (speed_kmh) ), size = .1) +
  #   geom_point(data = ais_data %>%
  #                filter(id_ais_data_initial %in% strange_speed), aes(x = lon, y = lat), color = "red", size = .1) +
  #   scale_color_viridis_c()

  short_time <- sort(ais_data$id_ais_data_initial[!(ais_data$high_speed) &
                                                    !(ais_data$station) &
                                                    # ais_data$time_travelled < 600 &
                                                    ais_data$id_mmsi_point_initial != 1 &
                                                    ((ais_data$distance_travelled <= 1 & ais_data$time_travelled > 60) |
                                                       ais_data$distance_travelled == 0)])

  # short_time <- na.omit(purrr::map_dbl(short_time, function(st) {
  #   length_in <- which(ais_data$mmsi[st + 1:5] == ais_data$mmsi[st]) ## extract following points having the same mmsi.
  #
  #   out <- ifelse(!all(ais_data$id_ais_data_initial[(st + 1:5)[length_in]] %in% short_time
  #                      ## if all following points with same mmsi are inside short time, return NA
  #                      ## if no following point have the same mmsi, return NA
  #                      ## if any following point with same mmsi are not inside short time, return st
  #                      ),
  #                 st,
  #                 NA)
  #   # cat(st, "   ", match(st, short_time), "/", length(short_time), "    ", out, "\n")
  #   return(out)
  # }))
  #
  # short_time <- sort(unique(na.omit(do.call("c", purrr::map(1:5, function(it) {
  #   print(it)
  #   out <- short_timec[ais_data$id_ais_data_initial[short_timec + it] != c(short_timec[1 + it:(length(short_timec)-1)], rep(0, it)) &
  #                        ais_data$mmsi[short_timec + it] == ais_data$mmsi[short_timec]]
  #
  #   return(out)
  # })))))

  strange_speed <- sort(unique(c(strange_speed, short_time)))
  it_sp <- 0

  while (length(strange_speed) > 0 & it_sp < nb_iteration) {
    # cat(it_sp, "strange speed:", length(strange_speed), "higher than", "--------------------------------------------------------\n")
    nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .001 &
                     !((ais_data$distance_travelled[strange_speed] <= 1 & ais_data$time_travelled[strange_speed] > 30) |
                         ais_data$distance_travelled[strange_speed] == 0) &
                     ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
                     !ais_data$station[strange_speed - 1])
    if (length(nextp) > 0) {
      strange_speed[nextp] <- strange_speed[nextp] - 1
      strange_speed <- unique(strange_speed)
    }

    nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .001 &
                     ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
                     !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
                     !ais_data$station[strange_speed - 1])
    it_strange_sp <- 0

    while (length(nextp) > 0 & it_strange_sp < nb_iteration_successive_strange) {
      # cat(it_strange_sp, "      next to add:", length(nextp), "     stange speed:", length(strange_speed), " IN  ", it_sp, "\n")
      strange_speed <- sort(unique(c(strange_speed, strange_speed[nextp] - 1)))
      nextp <- which(ais_data$speed_kmh[strange_speed - 1] < .001 &
                       ais_data$id_mmsi_point_initial[strange_speed - 1] != 1 &
                       !(ais_data$id_ais_data_initial[strange_speed - 1] %in% strange_speed) &
                       !ais_data$station[strange_speed - 1])
      it_strange_sp <- it_strange_sp + 1
    }

    to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ] %>%
      dplyr::arrange(id_ais_data_initial)

    mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]

    to_correct <- to_correct %>%
      dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
      dplyr::mutate(time_travelled = timestamp - c(dplyr::first(timestamp), timestamp[-n()]),
                    time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                    distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                    speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
      ) %>%
      dplyr::select(-c("tmmsi"))

    to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
      dplyr::mutate(speed_kmh_corrected = T)

    ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
      dplyr::mutate(speed_kmh_corrected = F) %>%
      rbind(to_correct) %>%
      dplyr::arrange(id_ais_data_initial)

    rm(to_correct)
    rm(strange_speed)

    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())

    strange_speed <- ais_data %>%  # [(ais_data$speed_kmh > threshold_speed_to_correct | ais_data$speed_kmh >) &
      dplyr::group_by(mmsi) %>%
      dplyr::mutate(threshold_strange_speed = threshold_speed_to_correct_expr(speed_kmh)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(id_mmsi_point_initial != 1) %>%
      dplyr::mutate(threshold_strange_speed = ifelse(is.null(threshold_strange_speed) | is.na(threshold_strange_speed) | is.nan(threshold_strange_speed),
                                                     threshold_speed_to_correct,
                                                     threshold_strange_speed)) %>%
      dplyr::filter((speed_kmh > threshold_speed_to_correct |
                       speed_kmh > threshold_strange_speed) &
                      (time_travelled < time_stop &
                         !(high_speed) &
                         !(station)))

    # thresh <- unique(strange_speed$threshold_strange_speed)

    strange_speed <- strange_speed[c(T, (strange_speed$id_ais_data_initial[-1] - strange_speed$id_ais_data_initial[-nrow(strange_speed)]) > 2), ]

    strange_speed <- strange_speed$id_ais_data_initial

    # ggplot() +
    #   geom_point(data = ais_data_ref %>%
    #                filter(mmsi == 265719460) %>%
    #                filter(id_mmsi_point_initial %in% 18700:19000) %>%
    #                mutate(id_mmsi_point_initial = 1:n()), aes( x = lon, y = lat), color = "orange") +
    #   geom_point(data = ais_data, aes(x = lon, y = lat, color = (speed_kmh) ), size = 1) +
    #   geom_point(data = ais_data %>%
    #                filter(id_ais_data_initial %in% strange_speed), aes(x = lon, y = lat), color = "red", size = 1) +
    #   scale_color_viridis_c()

    short_time <- sort(ais_data$id_ais_data_initial[!(ais_data$high_speed) &
                                                      !(ais_data$station) &
                                                      # ais_data$time_travelled < 600 &
                                                      ais_data$id_mmsi_point_initial != 1 &
                                                      ((ais_data$distance_travelled <= 1 & ais_data$time_travelled > 60) |
                                                         ais_data$distance_travelled == 0)])

    strange_speed <- sort(unique(c(strange_speed, short_time)))

    it_sp <- it_sp + 1
  }

  # mms <- unique(ais_data_ref$mmsi)[15]
  # mms <- 219017843
  #
  # ggplot() +
  #   geom_path(data = ais_data_ref %>%
  #                filter(mmsi == mms), aes(x = lon, y = lat), color = "red") +
  #   geom_point(data = ais_data_ref %>%
  #                filter(mmsi == mms), aes(x = lon, y = lat), color = "red", size = 1) +
  #   geom_path(data = ais_data %>%
  #                filter(mmsi == mms), aes(x = lon, y = lat)) +
  #   geom_point(data = ais_data %>%
  #                filter(mmsi == mms), aes(x = lon, y = lat), size = 1)
  #
  # ggplot() +
  #   geom_path(data = map_dfr(list(ais_data_ref %>%
  #                                   filter(mmsi == mms) %>%
  #                                   mutate(type = "initial"),
  #                                 ais_data %>%
  #                                   filter(mmsi == mms) %>%
  #                                   mutate(type = "new")), function(f) {return(f)}), aes(x = lon, y = lat)) +
  #   geom_point(data = map_dfr(list(ais_data_ref %>%
  #                                    filter(mmsi == mms) %>%
  #                                    mutate(type = "initial"),
  #                                  ais_data %>%
  #                                    filter(mmsi == mms) %>%
  #                                    mutate(type = "new")), function(f) {return(f)}), aes(x = lon, y = lat)) +
  # facet_wrap(~type)

  # miss <- ais_data_ref %>%
  #   filter(mmsi == mms)
  # new <- ais_data  %>%
  #   filter(mmsi == mms)

  if (length(strange_speed) > 0) {
    to_correct <- ais_data[!(ais_data$id_ais_data_initial %in% strange_speed) & ais_data$id_ais_data_initial %in% c(strange_speed - 1, strange_speed + 1), ]

    mmsi_prev <- to_correct$mmsi[-nrow(to_correct)]

    to_correct <- to_correct %>%
      dplyr::mutate(tmmsi = c("initial", mmsi_prev)) %>%
      dplyr::mutate(time_travelled = timestamp - c(dplyr::first(timestamp), timestamp[-n()]),
                    time_travelled = ifelse(time_travelled > time_stop | mmsi != tmmsi | (is.na(mmsi) & !is.na(tmmsi)) | (!is.na(mmsi) & is.na(tmmsi)), 0, time_travelled),
                    distance_travelled = ifelse(time_travelled == 0, 0, c(0, sqrt((X[-n()] - X[-1])^2 + (Y[-n()] - Y[-1])^2))),
                    speed_kmh = ifelse(time_travelled == 0, 0, c(0, distance_travelled[-1] * 60 * 60 / (1000 * time_travelled[-1])))
      ) %>%
      dplyr::select(-c("tmmsi"))

    to_correct <- to_correct[to_correct$id_ais_data_initial %in% (strange_speed + 1), ] %>%
      dplyr::mutate(speed_kmh_corrected = T)

    ais_data <- ais_data[!(ais_data$id_ais_data_initial %in% c(strange_speed, strange_speed + 1)), ] %>%
      dplyr::mutate(speed_kmh_corrected = F) %>%
      rbind(to_correct) %>%
      dplyr::arrange(id_ais_data_initial)

    rm(to_correct)
    rm(strange_speed)

    ais_data <- ais_data %>%
      dplyr::mutate(id_ais_data_initial = 1:n())
  }

  if ("real_high_speed" %in% colnames(ais_data)) {
    ais_data <- ais_data %>%
      dplyr::mutate(high_speed = real_high_speed) %>%
      dplyr::select(-real_high_speed)
  }

  filt <- unique(c(init_cols, "speed_kmh_corrected", "time_travelled", "distance_travelled", "speed_kmh", "station", "high_speed"))
  filt <- filt[filt %in% colnames(ais_data)]

  ais_data <- ais_data %>%
    dplyr::select(dplyr::all_of(filt))

  return(ais_data)
}
