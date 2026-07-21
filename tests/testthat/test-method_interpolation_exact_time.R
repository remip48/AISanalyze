library(AISanalyze)
library(testthat)

test_that("method_interpolation_exact_time", {

  ## create AIS data separated by 60 seconds
  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  ## create a target location and timestamp in between ais_data point 49 and 50
  target_timestamp_location <- ais_data[49, ] %>%
    dplyr::mutate(timestamp = (timestamp + ais_data$timestamp[50]) / 2)

  ## interpolate ais_data at this exact timestamp
  out <- method_interpolation_exact_time(ais_data,
                                         data = target_timestamp_location %>%
                                           dplyr::select(timestamp, lon, lat) %>%
                                           add_coordinates_meters(),
                                         crs_meters = 3035,
                                         radius = 200000,
                                         timestamp_to_interpolate = unique(target_timestamp_location$timestamp),
                                         nb_cores = 1,
                                         outfile = "log.txt")

  ## check
  expect_equal(out$timestamp, (ais_data$timestamp[49] + ais_data$timestamp[50]) / 2)
  expect_equal(out$lon, (ais_data$lon[49] + ais_data$lon[50]) / 2)
  expect_equal(out$lat, (ais_data$lat[49] + ais_data$lat[50]) / 2)

  ## delete log file
  unlink(r"(tests\testthat\log.txt)")

})
