test_that("spot check", {
  max <- 100
  expect_equal(hill_conc( max /  2,  max, 10, 1),  10)
  expect_equal(hill_conc( max /  4,  max,  9, 1),   3)
  expect_equal(hill_conc(-max / 10, -max,  9, 1),   1)
  expect_equal(hill_conc( max /  5,  max, 10, 2),   5)
})

test_that("corner cases", {
  expect_equal(hill_conc(0:2, 2, 5, 1), c(0, 5, Inf))
})

test_that("negatives", {
  expect_error(hill_conc(-1,  1,  1, 1))
  expect_error(hill_conc( 1, -1,  1, 1))
  expect_error(hill_conc( 1,  1, -1, 1))
  expect_error(hill_conc(c(1, 1), c(-1, 1), 1, 1))
})
