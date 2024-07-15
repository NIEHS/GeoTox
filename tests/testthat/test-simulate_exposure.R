test_that("bad inputs", {
  # Input should be a data frame
  expect_error(simulate_exposure(c()))
  # Need columns named by "expos_mean" and "expos_sd"
  expect_error(simulate_exposure(data.frame(x = 0, y = 0)))
  # Need column named by "expos_label" if nrows > 1
  expect_error(simulate_exposure(data.frame(mean = c(0, 0), sd = c(0, 0))))
})

test_that("expected output", {
  
  x <- data.frame(ave = 1:6,
                  stdev = 0.1,
                  label = rep(c("c", "a", "b"), 2),
                  loc = rep(c("B", "A"), each = 3))
  exposure <- simulate_exposure(split(x, ~loc),
                              expos_mean = "ave",
                              expos_sd = "stdev",
                              expos_label = "label",
                              n = 5)
  
  expect_equal(dim(exposure[[1]]), c(5, 3))
  expect_equal(names(exposure), c("A", "B"))
  # Output data.frame columns should be sorted by "expos_label"
  expect_equal(colnames(exposure[[1]]), c("a", "b", "c"))

})
