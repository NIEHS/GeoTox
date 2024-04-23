test_that("NAs", {
  expect_equal(simulate_inhalation_rate(c(-1, 101)), list(NA, NA))
  expect_equal(simulate_inhalation_rate(c(1, 2, "a")), list(NA, NA, NA))
})
