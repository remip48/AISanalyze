library(AISanalyze)
library(testthat)

test_that("AIScorrect_speed", {

  ## create a correct vessel track
  data <- data.frame(timestamp = 60 * 1:100,
                     lon = seq(5, 5.1, length = 100),
                     lat = seq(5, 5.1, length = 100),
                     mmsi = 1)

  ## Error 1: generate delayed GPS reception
  data[10, c("lon", "lat")] <- data[9, c("lon", "lat")]

  # Error 2:
  data[c(50:53), c("lon", "lat")] <- data[49, c("lon", "lat")]

  ## Error 3: generate GPS error
  data[20, ] <- data.frame(timestamp = data$timestamp[20],
                           lon = data$lon[20] - 5,
                           lat = data$lon[20] + 5,
                           mmsi = 1)

  # Add an "arrival at harbour" at the end, where the vessel remains stationary.
  # These points are detected as GPS delays because the algorithm cannot distinguish
  # between GPS delays and a vessel that has stopped. To avoid overcorrecting
  # stationary vessels, only the first delayed GPS point is removed. As a result,
  # some delayed points may remain after correction, but stationary vessel data are preserved.
  # Error 4: (uncorrect)
  for (i in 1:10) {
    data <- rbind(data,
                  data[nrow(data), ] %>%
                    dplyr::mutate(timestamp = timestamp + 1*60))
  }

  ## simulate a new departure from harbour again
  for (i in 1:10) {
    data <- rbind(data,
                  data[nrow(data), ] %>%
                    dplyr::mutate(timestamp = timestamp + 1*60,
                                  lon = lon + .001,
                                  lat = lat + .001))
  }

  ## identify each point to check later
  data <- data %>%
    dplyr::mutate(id = 1:dplyr::n())

  data_correct <- data

  ## prepare data
  data <- AIStravel(data)

  ## correct speeds
  out <- AIScorrect_speed(data)

  expect_equal(nrow(out),
               nrow(data_correct) - 4) # 4 errors point removed

  expect_all_false(any(out$id %in% c(10, 50, 20, 101)))

  ## check that the speeds of points following the detected errors have been corrected
  expect_all_true(all(round(out$speed_kmh[out$id %in% c(11, 51, 21, 102)],
                            2) %in% round(out$speed_kmh[out$id %in% c(12, 52, 22, 103)],
                                                        2)))

  ## and that this correction was noted in the new column `speed_kmh_corrected`
  expect_all_true(out$speed_kmh_corrected[out$id %in% c(11, 51, 21, 102)])

})
