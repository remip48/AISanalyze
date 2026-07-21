library(AISanalyze)
library(testthat)

test_that("rename_colums_ais", {

  init_ais_data <- data.frame(mmsi = 1,
                              timestamp = 1,
                              lon = 1,
                              lat = 1,
                              other_columns = "X",
                              other_columns2 = "Y")

  data <- data.frame(timestamp = 1,
                     lon = 1,
                     lat = 1,
                     other_columns = "X",
                     other_columns2 = "Y")

  ais_data <- rename_colums_ais(init_ais_data,
                                data)

  ## check
  expect_true(any(colnames(ais_data) == "mmsi"))
  expect_true(any(colnames(ais_data) == "timestamp"))
  ## check that all other columns that were present in both init_ais_data and data
  # were renamed as paste0("ais_", colnames), excepted mmsi and timestamp.
  expect_all_true(stringr::str_detect(colnames(ais_data)[colnames(ais_data) != "mmsi" & colnames(ais_data) != "timestamp"],
                                      stringr::fixed("ais_")))

})
