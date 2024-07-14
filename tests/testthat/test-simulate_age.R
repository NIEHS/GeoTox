test_that("bad inputs", {
  # Input should be a data frame or list of data frames
  expect_error(simulate_age(c()))
  # Expected column names
  expect_error(simulate_age(data.frame(x = 0, y = 0)))
  # Too few rows
  expect_error(simulate_age(data.frame(AGEGRP = 0, TOT_POP = 0)))
})

test_that("expected output", {

  x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  x$TOT_POP[c(1, 10)] <- 100 # populate only age range 40-44
  ages_test <- simulate_age(x, 10)[[1]]

  expect_vector(ages_test, size = 10)
  expect_true(all(ages_test >= 40 & ages_test < 45))

})
