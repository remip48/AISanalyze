library(AISanalyze)
library(testthat)

test_that("AISextract", {

  ## prepare AIS data and assign an id to check results
  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel() %>%
    dplyr::mutate(id = 1:dplyr::n())

  ## create a dataframe with target timestamp and location around which we will extract AIS data
  data <- data.frame(timestamp = 3330,
                     lon = 5.05,
                     lat = 5.05)

  ## extract all vessel locations within a radius of 500 meters around this point
  # within a time interval of -5 min -> +5min
  out <- AISextract(data,
                    crs_meters = 3035,
                    ais_data,
                    return_all_vessel_locations = T,
                    search_into_radius_m = 500,
                    interval_time_before = 5 * 60,
                    interval_time_after = 5 * 60)

  ## check which AIS data are actually within the radius and the time interval:
  actually_inside <- ais_data %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs = 3035) %>%
    dplyr::mutate(distance_to_data = sf::st_distance(.,
                                                 data %>%
                                                   sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                                                   sf::st_transform(crs = 3035)) %>%
                                    units::drop_units() %>%
                                    as.vector()) %>%
    dplyr::filter(distance_to_data <= 500) %>%
    dplyr::filter(timestamp >= (data$timestamp - 5*60) &
                    timestamp <= (data$timestamp + 5*60))

  ## check
  expect_all_true(out$id %in% actually_inside$id)
  expect_all_true(round(out$distance_vessel_to_location_m, 0) %in% round(actually_inside$distance_to_data, 0))

  ## extract now the vessel location at the exact data$timestamp, if the position falls
  # within the target radius and time interval
  ais_data_interpolated <- AISinterpolate(ais_data,
                                          type_interpolation = "exact_timestamp",
                                          exact_timestamp = list(timestamp_to_interpolate = data$timestamp,
                                                                 locations_of_interest = data.frame(lon = data$lon,
                                                                                                    lat = data$lat),
                                                                 radius = 200000))

  unlink(r"(tests\testthat\log.txt)")

  out <- AISextract(data,
                    crs_meters = 3035,
                    ais_data_interpolated,
                    return_all_vessel_locations = FALSE,
                    search_into_radius_m = 500,
                    interval_time_before = 5 * 60,
                    interval_time_after = 5 * 60)

  actually_inside <- ais_data_interpolated %>%
    dplyr::filter(timestamp == data$timestamp) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs = 3035) %>%
    dplyr::mutate(distance_to_data = sf::st_distance(.,
                                                     data %>%
                                                       sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                                                       sf::st_transform(crs = 3035)) %>%
                    units::drop_units() %>%
                    as.vector()) %>%
    dplyr::filter(distance_to_data <= 500)

  ## check: no vessel extracted as this was outside the 500 meters radius at data$timestamp
  expect_all_true(length(na.omit(out$mmsi)) == nrow(actually_inside))

  ## now increase the radius to extract the vessel location
  out <- AISextract(data,
                    crs_meters = 3035,
                    ais_data_interpolated,
                    return_all_vessel_locations = FALSE,
                    search_into_radius_m = 1000,
                    interval_time_before = 5 * 60,
                    interval_time_after = 5 * 60)

  actually_inside <- ais_data_interpolated %>%
    dplyr::filter(timestamp == data$timestamp) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    sf::st_transform(crs = 3035) %>%
    dplyr::mutate(distance_to_data = sf::st_distance(.,
                                                     data %>%
                                                       sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                                                       sf::st_transform(crs = 3035)) %>%
                    units::drop_units() %>%
                    as.vector()) %>%
    dplyr::filter(distance_to_data <= 1000)

  ## check
  expect_all_true(out$id %in% actually_inside$id)
  expect_all_true(round(out$distance_vessel_to_location_m, 0) %in% round(actually_inside$distance_to_data, 0))

})
