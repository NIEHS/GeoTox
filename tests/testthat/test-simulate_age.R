test_that("inputs", {
  expect_error(simulate_age(data.frame()))
  expect_error(simulate_age(data.frame(AGEGRP = 0, TOT_POP = 0)))
})
