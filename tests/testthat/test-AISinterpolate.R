library(AISanalyze)
library(testthat)

test_that("AISinterpolate", {

  ## create AIS data separated by 60 seconds
  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  ## interpolate every 30 seconds
  out <- AISinterpolate(ais_data,
                        type_interpolation = "maximum_time_interval",
                        maximum_time_interval = list(maximum_gap_seconds = 30),
                        nb_cores = 1)

  ## check
  expect_all_true(out$timestamp %in% seq(dplyr::first(ais_data$timestamp),
                                         dplyr::last(ais_data$timestamp),
                                         by = 30))

  expect_all_true(round(out$lon, 5) %in% round(seq(dplyr::first(ais_data$lon),
                                                   dplyr::last(ais_data$lon),
                                                   length = nrow(out)), 5))

  expect_all_true(round(out$lat, 5) %in% round(seq(dplyr::first(ais_data$lat),
                                                   dplyr::last(ais_data$lat),
                                                   length = nrow(out)), 5))

  ## create a target location and timestamp in between ais_data point 49 and 50
  target_timestamp_location <- ais_data[49, ] %>%
    dplyr::mutate(timestamp = (timestamp + ais_data$timestamp[50]) / 2)

  ## interpolate ais_data at this exact timestamp
  out <- AISinterpolate(ais_data,
                        type_interpolation = "exact_timestamp",
                        exact_timestamp = list(timestamp_to_interpolate = target_timestamp_location$timestamp,
                                               locations_of_interest = data.frame(lon = target_timestamp_location$lon,
                                                                                  lat = target_timestamp_location$lat),
                                               radius = 200000),
                        nb_cores = 1)

  ## check
  expect_equal(out$timestamp, (ais_data$timestamp[49] + ais_data$timestamp[50]) / 2)
  expect_equal(out$lon, (ais_data$lon[49] + ais_data$lon[50]) / 2)
  expect_equal(out$lat, (ais_data$lat[49] + ais_data$lat[50]) / 2)

  ## delete log file
  unlink(r"(tests\testthat\log.txt)")

})
