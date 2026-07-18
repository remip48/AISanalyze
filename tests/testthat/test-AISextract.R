library(AISanalyze)
library(testthat)

test_that("AISextract", {

  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  data <- ais_data[50, ]

  out <- AISextract(data,
                    crs_meters = 3035,
                    ais_data,
                    search_into_radius_m = 500,
                    duplicate_time = F,
                    max_time_diff = 10 * 60,
                    t_gap = 10*60,
                    accelerate = F,
                    average_at = 0)

  expect_equal(out$ais_timestamp, ais_data$timestamp[50])

  out <- AISextract(data,
                    crs_meters = 3035,
                    ais_data,
                    search_into_radius_m = 5000,
                    duplicate_time = T,
                    max_time_diff = 10 * 60,
                    t_gap = 1 * 60,
                    accelerate = F,
                    average_at = 0)

  expect_all_true(out$ais_timestamp %in% seq(ais_data$timestamp[50], ais_data$timestamp[50] - 10*60, -60))

})
