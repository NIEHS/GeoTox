test_that("simulate_age returns errors from bad inputs", {
  # errors from bad input
  expect_error(simulate_age(data.frame()))
  # errors from bad order
  expect_error(simulate_age(data.frame(AGEGRP = 0, TOT_POP = 0)))
  #
})


test_that("age samples are good", {

  x <- data.frame(AGEGRP = 0:18, TOT_POP = c(sum(1:18), 1:18))
  ages_test <- simulate_age(x, 10)


  # age sample is of size "n"
  expect_vector(ages_test,size = 10)

  # age samples are within allowed age range [0,90] t
  expect_true(all(ages_test < 90 & ages_test >= 0))


  #
})
