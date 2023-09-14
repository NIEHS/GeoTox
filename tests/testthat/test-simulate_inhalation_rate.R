test_that("NAs", {
  expect_equal(simulate_inhalation_rate(c(-1, 101)), c(NA, NA))
  expect_equal(simulate_inhalation_rate(c(1, 2, "a")), c(NA, NA, NA))
})
