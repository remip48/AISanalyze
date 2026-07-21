library(AISanalyze)
library(testthat)

test_that("rename_columns_data works", {

  init_data <- data.frame(mmsi = 1,
                          other_columns = "X",
                          other_columns2 = "Y")

  data <- rename_columns_data(init_data)

  ## check
  expect_true(all(colnames(data) != "mmsi"))
  expect_true(all(colnames(init_data)[colnames(init_data) != "mmsi"] %in% colnames(data)))

})
