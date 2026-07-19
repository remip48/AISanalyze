library(AISanalyze)
library(testthat)

test_that("AISinfos", {

  set.seed(20260719)

  ais <- data.frame(mmsi = 1,
                    shiptype = sample(c(rep("Cargo", 85),
                                        rep("Unkown", 15))),
                    name = sample(c(rep("Fastest boat", 85),
                                    rep("Unkown", 15))),
                    imo = sample(c(rep(000, 85),
                                   rep("Unkown", 15))),
                    length = sample(c(rep(55, 80),
                                      rep(1000, 5),
                                      rep("Unkown", 15))),
                    width = sample(c(rep(30, 80),
                                     rep(100, 5),
                                     rep("Unkown", 15))),
                    draught = sample(c(rep(5, 80),
                                       rep(50, 5),
                                       rep("Unkown", 15))))

  out <- AISinfos(ais)

  expect_true(out$values_obtained$Selected_shiptype == "Cargo")
  expect_true(out$values_obtained$Selected_name == "Fastest boat")
  expect_true(out$values_obtained$Selected_imo == 000)
  expect_true(out$values_obtained$Selected_length == 55)
  expect_true(out$values_obtained$Selected_width == 30)
  expect_true(out$values_obtained$Selected_draught == 5)
  expect_true(out$summary$n_point_mmsi == 100)
  expect_true(out$summary$n_point_with_length_value == 80)

})
