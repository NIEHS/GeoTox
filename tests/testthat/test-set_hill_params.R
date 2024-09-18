test_that("setting", {
  
  hill_params <- "test"
  
  x <- list()
  
  expect_no_warning(x <- set_hill_params(x, hill_params))
  expect_equal(x, list(hill_params = hill_params))
  
})

test_that("clear downstream", {
  
  hill_params <- "test"
  
  x <- list(resp = "test")
  
  expect_warning(x <- set_hill_params(x, hill_params))
  expect_equal(x, list(hill_params = hill_params))
  
  x <- list(sensitivity = "test")

  expect_warning(x <- set_hill_params(x, hill_params))
  expect_equal(x, list(hill_params = hill_params))
  
})
