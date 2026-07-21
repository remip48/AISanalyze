library(AISanalyze)
library(testthat)

test_that("AIStravel", {

  ## create an AIS track
  ais_data <- data.frame(timestamp = 60 * 1:100,
                         lon = seq(5, 5.1, length = 100),
                         lat = seq(5, 5.1, length = 100),
                         mmsi = 1) %>%
    AIStravel()

  out <- AIStravel(ais_data)

  ## check actual distance and speed travelled:
  dst_travelled <- sf::st_distance(ais_data[-1,] %>%
                            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                            sf::st_transform(crs = 3035),
                          ais_data[-nrow(ais_data),] %>%
                            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                            sf::st_transform(crs = 3035),
                          by_element = TRUE)

  speed_travelled <- c(0,
                       (
                         dst_travelled / 60 # in meter / second
                       ) * 3600 / 1000 # to convert in km/h
  )

  ## check
  expect_all_true(ais_data$distance_travelled == c(0, dst_travelled))
  expect_all_true(out$time_travelled == c(0, rep(60, nrow(ais_data) - 1)))
  expect_all_true(round(out$speed_kmh, 0) %in% round(speed_travelled, 0))

})
