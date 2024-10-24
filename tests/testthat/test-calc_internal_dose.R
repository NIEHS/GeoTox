test_that("inputs", {
  
  expect_no_error(calc_internal_dose(C_ext = matrix(1), IR = 2))
  expect_no_error(calc_internal_dose(C_ext = matrix(1), IR = as.integer(2)))
  expect_no_error(calc_internal_dose(C_ext = list(matrix(1)), IR = list(2)))
  
  expect_error(calc_internal_dose(C_ext = 1, IR = 2))
  expect_error(calc_internal_dose(C_ext = list(matrix(1), c(1)), IR = 2))
  expect_error(calc_internal_dose(C_ext = matrix(1), IR = "a"))
  expect_error(calc_internal_dose(C_ext = matrix(1), IR = list(2, "a")))
})

test_that("output dimensions", {
  
  # Single value
  
  out <- calc_internal_dose(C_ext = matrix(1), IR = 2)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(lapply(out, dim),
               list(c(1, 1)))

  # Single population
  
  C_ext <- matrix(1:15, ncol = 3)
  IR <- 1:5
  
  out <- calc_internal_dose(C_ext, IR)

  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(lapply(out, dim),
               list(c(5, 3)))
  
  # Multiple populations
  
  C_ext <- list(
    "a" = matrix(1:15 / 10, ncol = 3),
    "b" = matrix(1:8, ncol = 2)
  )
  IR <- list(1:5, 1:4 / 2)
  
  out <- calc_internal_dose(C_ext, IR)
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(lapply(out, dim),
               list(a = c(5, 3), b = c(4, 2)))
})
