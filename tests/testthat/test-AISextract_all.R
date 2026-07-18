library(AISanalyze)
library(testthat)

test_that("AISextract_all", {

  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  data <- data.frame(timestamp = 3360,
                     lon = 5.05,
                     lat = 5.05)

  ## extract
  out <- AISextract_all(data,
                        crs_meters = 3035,
                        ais_data,
                        search_into_radius_m = 500,
                        interval_time_before = 5 * 60,
                        interval_time_after = 5 * 60)

  expect_all_true(out$ais_timestamp %in% c(3060, 3120, 3180))

})
