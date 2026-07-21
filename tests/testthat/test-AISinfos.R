library(AISanalyze)
library(testthat)

test_that("AISinfos", {

  set.seed(20260719)

  ## create 100 points of AIS data with incomplete information in shiptype, name,
  # imo, length, width and draught
  ais <- data.frame(mmsi = 1,
                    shiptype = sample(c(rep("Cargo", 85), # correct one
                                        rep("Unkown", 15))),
                    name = sample(c(rep("Fastest boat", 85), # correct one
                                    rep("Unkown", 15))),
                    imo = sample(c(rep(000, 85), # correct one
                                   rep(NA, 15))),
                    length = sample(c(rep(55, 80), # correct one
                                      rep(1000, 5),
                                      rep(NA, 15))),
                    width = sample(c(rep(30, 80), # correct one
                                     rep(100, 5),
                                     rep(NA, 15))),
                    draught = sample(c(rep(5, 80), # correct one
                                       rep(50, 5),
                                       rep(NA, 15))))

  out <- AISinfos(ais)

  ## check selected values are correct
  expect_true(out$estimated_values$Selected_shiptype == "Cargo")
  expect_true(out$estimated_values$Selected_name == "Fastest boat")
  expect_true(out$estimated_values$Selected_imo == 000)
  expect_true(out$estimated_values$Selected_length == 55)
  expect_true(out$estimated_values$Selected_width == 30)
  expect_true(out$estimated_values$Selected_draught == 5)
  expect_true(out$summary$n_point_mmsi == 100)
  expect_true(out$summary$n_point_with_length_value == 80)

})
