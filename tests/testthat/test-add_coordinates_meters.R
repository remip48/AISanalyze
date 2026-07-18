library(AISanalyze)
library(testthat)

test_that("add_coordinates_meters works", {

  data <- data.frame(lon = 5, lat = 5)

  out <- add_coordinates_meters(data)

  expected <- data %>%
    dplyr::mutate(tlon = lon, tlat = lat) %>%
    st_as_sf(coords = c("tlon", "tlat"), crs = 4326) %>%
    st_transform(crs = 3035) %>%
    dplyr::mutate(X = st_coordinates(.)[,1],
                  Y = st_coordinates(.)[,2]) %>%
    st_cast()

  expect_equal(as.vector(out$X),
               as.vector(expected$X))

  expect_equal(as.vector(out$Y),
               as.vector(expected$Y))
})
