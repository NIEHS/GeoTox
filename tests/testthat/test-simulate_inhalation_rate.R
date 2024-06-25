test_that("NAs", {
  expect_equal(simulate_inhalation_rate(c(-1, 101)), list(c(NA, NA)))
  expect_equal(simulate_inhalation_rate(c(1, 2, "a")), list(c(NA, NA, NA)))
})

test_that("bad inputs", {
  expect_error(simulate_inhalation_rate(c()))
  expect_error(simulate_inhalation_rate(0:5, IR_params = 0))
})

test_that("expected output", {
  
  IR_params <- data.frame("age" = c(0, 20, 50),
                          "mean" = c(0.5, 0.3, 0.2),
                          "sd" = 0)
  IR_test <- simulate_inhalation_rate(c(5, 25, 55), IR_params)[[1]]
  
  expect_vector(IR_test, size = 3)
  expect_true(all(IR_test == c(0.5, 0.3, 0.2)))
})
