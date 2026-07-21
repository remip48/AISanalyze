library(AISanalyze)
library(testthat)

test_that("AISidentify_stations_aircraft", {

  ## create a dataset with an high-speed craft (with speed ~ 141 km/h) and a station (e.g. buoy)
  # that move from about 1 meter per hour
  ais_data <- rbind(data.frame(timestamp = 60 * 1:100,
                               lon = seq(5, 6.5, length = 100),
                               lat = seq(5, 6.5, length = 100),
                               mmsi = "aircraft"),
                    data.frame(timestamp = 60 * 1:100,
                               lon = 5,
                               lat = 5,
                               mmsi = "buoy")) %>%
    AIStravel() %>%
    dplyr::mutate(distance_travelled = ifelse(mmsi == "buoy",
                                              1 / 60,
                                              distance_travelled))

  ## identify
  out <- AISidentify_stations_aircraft(ais_data)

  ## check
  expect_all_true(out$high_speed[out$mmsi == "aircraft"])
  expect_all_true(out$station[out$mmsi == "buoy"])

})
