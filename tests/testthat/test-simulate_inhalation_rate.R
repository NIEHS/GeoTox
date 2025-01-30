test_that("NAs", {
  expect_equal(simulate_inhalation_rate(c(-1, 101)), list(c(NA, NA)))
})

test_that("bad inputs", {
  expect_error(simulate_inhalation_rate(c()),
               "`x` must be a numeric vector or list of numeric vectors")
  expect_error(simulate_inhalation_rate(c(1, 2, "a")),
               "`x` must be a numeric vector or list of numeric vectors")
  expect_error(simulate_inhalation_rate(0:5, IR_params = 0),
               "`IR_params` must contain columns `age`, `mean` and `sd`")
})

test_that("single numeric vector", {
  
  x <- c(0, 1, 2, 5, 7, 11, 23, 30, 40, 65)
  
  out <- simulate_inhalation_rate(x)

  expect_type(out, "list")
  expect_length(out, 1)
  expect_vector(out[[1]], ptype = double(), size = 10)

})

test_that("two numeric vectors", {
  
  x <- c(0, 1, 2, 5, 7, 11, 23, 30, 40, 65)
  y <- seq(0, 80, by = 10)
  
  out <- simulate_inhalation_rate(list(x, y))
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_vector(out[[1]], ptype = double(), size = 10)
  expect_vector(out[[2]], ptype = double(), size = 9)
  
})

test_that("custom IR_params", {
  
  x <- c(5, 25, 55)
  # Have rows out of order, will be sorted in function
  IR_params <- data.frame("age" = c(20, 0, 50),
                          "mean" = c(0.3, 0.5, 0.2),
                          "sd" = 0)
  
  out <- simulate_inhalation_rate(x, IR_params)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(out[[1]], c(0.5, 0.3, 0.2))
  
})


test_that("internal", {
  
  x <- c(5, 25, 55)
  IR_params <- data.frame("age" = c(0, 20, 50),
                          "mean" = c(0.5, 0.3, 0.2),
                          "sd" = 0)
  
  out <- .simulate_inhalation_rate(x, IR_params)
  
  expect_vector(out, ptype = double(), size = 3)
  expect_equal(out, c(0.5, 0.3, 0.2))
  
})
