test_that("spot check", {
  max <- 100
  expect_equal(hill_val(10,  max, 10, 1),  max /  2)
  expect_equal(hill_val( 3,  max,  9, 1),  max /  4)
  expect_equal(hill_val( 1, -max,  9, 1), -max / 10)
  expect_equal(hill_val( 5,  max, 10, 2),  max /  5)
})

test_that("corner cases", {
  expect_equal(hill_val(c(0, 5, Inf), 2, 5, 1), 0:2)
})

test_that("negatives", {
  expect_error(hill_val(-1, 2,  1, 1))
  expect_error(hill_val( 1, 2, -1, 1))
})
