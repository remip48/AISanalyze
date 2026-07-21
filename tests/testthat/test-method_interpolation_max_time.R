library(AISanalyze)
library(testthat)

test_that("method_interpolation_max_time", {

  ## create AIS data separated by 60 seconds
  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  ## interpolate every 30 seconds
  out <- method_interpolation_max_time(ais_data,
                                       maximum_gap_seconds = 30)

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

})
