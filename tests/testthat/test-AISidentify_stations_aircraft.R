library(AISanalyze)
library(testthat)

test_that("AISidentify_stations_aircraft", {

  ais_data <- rbind(data.frame(timestamp = 60 * 1:100,
                               lon = seq(5, 6.5, length = 100),
                               lat = seq(5, 6.5, length = 100),
                               mmsi = 1),
                    data.frame(timestamp = 60 * 1:100,
                               lon = rnorm(100, 5, .0001),
                               lat = rnorm(100, 5, .0001),
                               mmsi = 2)) %>%
    AIStravel() %>%
    dplyr::mutate(distance_travelled = ifelse(mmsi == 2,
                                              0.01,
                                              distance_travelled))

  out <- AISidentify_stations_aircraft(ais_data,
                                       crs_meters = 3035,
                                       quantile_station = 0.975,
                                       threshold_distance_station = 1,
                                       quantile_high_speed = 0.97,
                                       threshold_high_speed = 110)

  expect_all_true(out$high_speed[out$mmsi == 1])
  expect_all_true(out$station[out$mmsi == 2])

})
