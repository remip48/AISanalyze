library(AISanalyze)
library(testthat)

test_that("AIScorrect_speed", {

  data <- data.frame(timestamp = 60 * 1:100,
                     lon = seq(5, 5.1, length = 100),
                     lat = seq(5, 5.1, length = 100),
                     mmsi = 1)

  ## add an "arrival to harbour" at the end, where the ship does not move
  for (i in 1:10) {
    data <- rbind(data,
                  data[nrow(data), ] %>%
                    dplyr::mutate(timestamp = timestamp + 1*60))
  }

  ## simulate departure from harbour again
  data <- rbind(data,
                data[nrow(data), ] %>%
                  dplyr::mutate(timestamp = timestamp + 1*60,
                                lon = lon + .001,
                                lat = lat + .001))

  data_correct <- data

  ## generate errors
  data[10, c("lon", "lat")] <- data[9, c("lon", "lat")]

  data[20, ] <- data.frame(timestamp = 60 * 20,
                           lon = 0,
                           lat = 0,
                           mmsi = 1)

  data[c(50:53), c("lon", "lat")] <- data[49, c("lon", "lat")]

  ## correct speeds
  out <- AIScorrect_speed(data %>%
                            AIStravel() %>%
                            AISidentify_stations_aircraft())

  expect_equal(nrow(out),
               nrow(data_correct) - 4)

  # expect_all_true(round(out$speed_kmh, 2) %in% c(9.44, 9.35, 0, 47.22))

  expect_all_true(out$speed_kmh_corrected[c(10, 19, 48, 98)])

})
