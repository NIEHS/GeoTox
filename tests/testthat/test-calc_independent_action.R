test_that("random spot check", {
  Emax <- 100
  conc <-  10 * runif(10)
  max  <- Emax * runif(10)
  AC50 <-  10 * runif(10)
  n    <- 1 + rpois(10, 5)
  IA   <- calc_independent_action(conc, max, AC50, Emax, n)
  
  expect_true(IA > 0 & IA < Emax)
})

# test_that("max > Emax", {
#   expect_warning(calc_independent_action(1, 100, 1, 10, 1))
# })
