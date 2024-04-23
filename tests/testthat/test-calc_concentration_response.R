test_that("mixture calculation for errors from bad inputs", {
  # errors from bad input

  # errors from bad order

})


test_that("calc_concentration_response is valid with simulated data", {


  # Set up a mixture concentration-response
  conc <- seq(0,1,by = 0.1)



  #
})

test_that("calc_concentration_response is valid with ICE data", {

  # Set up a mixture concentration-response from ICE
  ice_data <- geo_tox_data$ice

  conc <- seq(0,1,by = 0.1)



  #
})



test_that("calc_independent_action scales to Emax", {


  x <- data.frame(AGEGRP = 0:18, TOT_POP = c(sum(1:18), 1:18))
  ages_test <- simulate_age(x, 10)[[1]]


  # age sample is of size "n"
  expect_vector(ages_test,size = 10)

  # age samples are within allowed age range [0,90] t
  expect_true(all(ages_test < 90 & ages_test >= 0))


  #
})
