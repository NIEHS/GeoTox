test_that("inputs", {
  
  expect_no_error(calc_invitro_concentration(D_int = matrix(1),
                                             C_ss = 2))
  expect_no_error(calc_invitro_concentration(D_int = matrix(1),
                                             C_ss = as.integer(2)))
  expect_no_error(calc_invitro_concentration(D_int = matrix(1),
                                             C_ss = matrix(2)))
  expect_no_error(calc_invitro_concentration(D_int = list(matrix(1)),
                                             C_ss = list(2)))
  
  expect_error(calc_invitro_concentration(D_int = 1,
                                          C_ss = 2))
  expect_error(calc_invitro_concentration(D_int = list(matrix(1), c(1)),
                                          C_ss = 2))
  expect_error(calc_invitro_concentration(D_int = matrix(1),
                                          C_ss = "a"))
  expect_error(calc_invitro_concentration(D_int = matrix(1),
                                          C_ss = list(2, "a")))
  
  # C_ss is NULL
  expect_error(calc_invitro_concentration(D_int = matrix(1)))
})

test_that("output dimensions", {
  
  # Single value
  
  out <- calc_invitro_concentration(D_int = matrix(1), C_ss = 2)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(lapply(out, dim),
               list(c(1, 1)))
  
  # Single population
  
  D_int <- matrix(1:15, ncol = 3)
  C_ss <- 1:5
  
  out <- calc_invitro_concentration(D_int, C_ss)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(lapply(out, dim),
               list(c(5, 3)))
  
  # Multiple populations
  
  D_int <- list(
    "a" = matrix(1:15 / 10, ncol = 3),
    "b" = matrix(1:8, ncol = 2)
  )
  C_ss <- list(1:5, 1:4 / 2)
  
  out <- calc_invitro_concentration(D_int, C_ss)
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(lapply(out, dim),
               list(a = c(5, 3), b = c(4, 2)))
})
