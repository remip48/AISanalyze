library(AISanalyze)
library(testthat)

test_that("AIStravel", {

  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  out <- AIStravel(ais_data,
                   crs_meters = 3035)

  expect_all_true(round(out$speed_kmh, 2) %in% c(0, 9.44))
  expect_all_true(round(out$time_travelled, 2) %in% c(0, 60))
  expect_all_true(round(out$distance_travelled, 0) %in% c(0, 157))

})
