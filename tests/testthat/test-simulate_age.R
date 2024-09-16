test_that("bad inputs", {
  # Input should be a data frame or list of data frames
  expect_error(simulate_age(c()))
  # Expected column names
  expect_error(simulate_age(data.frame(x = 0, y = 0)))
  # Too few rows
  expect_error(simulate_age(data.frame(AGEGRP = 0, TOT_POP = 0)))
})

test_that("single data frame", {
  
  x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  # populate only age range 40-44, set population total of all ages
  x$TOT_POP[c(1, 10)] <- 100
  
  out <- simulate_age(x, 10)

  expect_type(out, "list")
  expect_length(out, 1)
  expect_vector(out[[1]], ptype = integer(), size = 10)
  expect_true(all(out[[1]] >= 40 & out[[1]] < 45))
  
})

test_that("two data frames", {
  
  x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  # populate only age range 40-44, set population total of all ages
  x$TOT_POP[c(1, 10)] <- 100
  
  y <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  # populate age ranges 5-9 and 50-54
  y$TOT_POP[c(3, 12)] <- 10
  # set population total for all age groups
  y$TOT_POP[1] <- sum(y$TOT_POP)
  
  out <- simulate_age(list(x = x, y = y), 15)
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_vector(out[[1]], ptype = integer(), size = 15)
  expect_vector(out[[2]], ptype = integer(), size = 15)
  expect_true(all(out[[1]] >= 40 & out[[1]] < 45))
  expect_true(all(
    (out[[2]] >= 5 & out[[2]] < 10) | (out[[2]] >= 50 & out[[2]] < 55)
  ))
  
})

test_that("internal", {
  
  x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  # populate only age range 40-44, set population total of all ages
  x$TOT_POP[c(1, 10)] <- 100
  
  out <- .simulate_age(x, 10)
  
  expect_vector(out, ptype = integer(), size = 10)
  expect_true(all(out >= 40 & out < 45))
  
})
