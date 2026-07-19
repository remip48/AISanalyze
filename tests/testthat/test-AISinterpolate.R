library(AISanalyze)
library(testthat)

test_that("AISinterpolate", {

  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  data <- data.frame(timestamp = 3360,
                     lon = 5.05,
                     lat = 5.05)

  ## extract
  out <- AISinterpolate(ais_data,
                        type_interpolation = "maximum_time_interval",
                        maximum_time_interval = list(maximum_gap_seconds = 30),
                        parallelize = FALSE)

  expect_all_true(out$timestamp %in% seq(first(ais_data$timestamp),
                                         last(ais_data$timestamp),
                                         by = 30))

  expect_all_true(round(out$lon, 5) %in% round(seq(first(ais_data$lon),
                                                   last(ais_data$lon),
                                                   length = 199), 5))

  expect_all_true(round(out$lat, 5) %in% round(seq(first(ais_data$lat),
                                                   last(ais_data$lat),
                                                   length = 199), 5))

  ## extract
  data <- ais_data[50, ] %>%
    dplyr::mutate(timestamp = (timestamp + ais_data$timestamp[49]) / 2)

  out <- AISinterpolate(ais_data,
                        type_interpolation = "exact_timestamp",
                        exact_timestamp = list(timestamp_to_interpolate = data$timestamp,
                                               locations_of_interest = data.frame(lon = data$lon,
                                                                                  lat = data$lat),
                                               radius = 200000),
                        parallelize = FALSE)

  expect_equal(out$timestamp, (ais_data$timestamp[49] + ais_data$timestamp[50]) / 2)
  expect_equal(out$lon, (ais_data$lon[49] + ais_data$lon[50]) / 2)
  expect_equal(out$lat, (ais_data$lat[49] + ais_data$lat[50]) / 2)

})
