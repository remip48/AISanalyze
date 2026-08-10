library(AISanalyze)
library(testthat)

test_that("rename_columns_data works", {

  init_data <- data.frame(mmsi = 1,
                          point = 10,
                          other_columns = "X",
                          other_columns2 = "Y")

  data <- rename_columns_data(init_data)

  ## check tests
  expect_true(all(!(colnames(data) %in% c("mmsi"))))
  expect_true(all(colnames(init_data)[!(colnames(init_data) %in% c("mmsi"))] %in% colnames(data)))

})
