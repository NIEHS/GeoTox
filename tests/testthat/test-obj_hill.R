# Testing the obj-hill functions
test_that("2 and 3 parameter models give accurate likelihood", {

## hill function for simulation
  hill_fun <- function(x, alpha, beta, theta){
    alpha / (1 + 10^(beta * (theta - x)))
  }

## Creating regular parameters
  tp <- 1
  log10_ga <- 1
  gw <- 2
  err <- 1

## 2-parameter model

  #simulate dose-response
  params2 <- c(tp, log10_ga, err)
  log10_conc <- seq(0.1,10,length.out = 50)
  resp <- hill_fun(log10_conc, tp, log10_ga, gw)

  # 2 parameter hill model expectation
  val <- obj_hill(params2, log10_conc, resp)
  expect_equal(val, -100.201, tolerance = 0.001)

## 2-parameter model

  #simulate dose-response
  params3 <- c(tp, log10_ga, gw, err)

  # 3 parameter hill model expectation
  params3 <- c(tp, log10_ga, gw, err)
  val <- obj_hill(params3, log10_conc, resp)
  expect_equal(val, -99.23463, tolerance = 0.001)
})
