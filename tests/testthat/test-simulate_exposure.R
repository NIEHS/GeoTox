test_that("bad inputs", {
  # Input should be a data frame
  expect_error(simulate_exposure(c()))
  # Need columns named by "expos_mean" and "expos_sd"
  expect_error(simulate_exposure(data.frame(x = 0, y = 0)))
  expect_error(simulate_exposure(list(data.frame(x = 0, y = 0),
                                      data.frame(mean = 0, sd = 0))))
  # Need column named by "expos_label" if nrows > 1
  expect_error(simulate_exposure(data.frame(mean = c(0, 0), sd = c(0, 0))))
})

test_that("single row", {
  
  out <- simulate_exposure(data.frame(mean = 10, sd = 1), n = 5)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(dim(out[[1]]), c(5, 1))
  expect_equal(colnames(out[[1]]), NULL)
  
})

test_that("single data frame", {
  
  out <- simulate_exposure(data.frame(mean = 1:3,
                                      sd = (1:3) / 10,
                                      casn = letters[1:3]),
                           n = 5)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(dim(out[[1]]), c(5, 3))
  expect_equal(colnames(out[[1]]), letters[1:3])
  
})
  
test_that("two data frames", {
  
  out <- simulate_exposure(
    list(loc1 = data.frame(mean = 1:3,
                           sd = (1:3) / 10,
                           casn = letters[1:3]),
         loc2 = data.frame(mean = 4:7,
                           sd = 0.1,
                           casn = letters[c(4, 7, 6, 5)])),
    n = 5)
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(names(out), c("loc1", "loc2"))
  expect_equal(dim(out[[1]]), c(5, 3))
  expect_equal(dim(out[[2]]), c(5, 4))
  expect_equal(colnames(out[[1]]), letters[1:3])
  expect_equal(colnames(out[[2]]), letters[4:7])
  
})

test_that("custom column names", {
  
  out <- simulate_exposure(
    split(data.frame(ave = 1:6,
                     stdev = 0.1,
                     label = rep(c("c", "a", "b"), 2),
                     loc = rep(c("B", "A"), each = 3)),
          ~loc),
    expos_mean = "ave",
    expos_sd = "stdev",
    expos_label = "label",
    n = 5)
  
  expect_type(out, "list")
  expect_length(out, 2)
  expect_equal(names(out), c("A", "B"))
  expect_equal(dim(out[[1]]), c(5, 3))
  expect_equal(dim(out[[2]]), c(5, 3))
  # Output data.frame columns should be sorted by "expos_label"
  expect_equal(colnames(out[[1]]), c("a", "b", "c"))
  expect_equal(colnames(out[[2]]), c("a", "b", "c"))
  
})

test_that("internal", {
  
  out <- .simulate_exposure(data.frame(mean = 1:3, sd = (1:3) / 10),
                            mean = "mean",
                            sd = "sd",
                            n = 5)
  
  # Output is a matrix, so type == "double" with 2 dimensions
  expect_type(out, "double")
  expect_equal(dim(out), c(5, 3))
  expect_true(all(out >= 0))
  
})

test_that("internal special cases", {
  
  out <- .simulate_exposure(data.frame(mean = c(), sd = c()),
                            mean = "mean",
                            sd = "sd",
                            n = 1)
  
  # Output is a matrix, so type == "double" with 2 dimensions
  expect_type(out, "double")
  expect_equal(dim(out), c(1, 0))
  
  out <- .simulate_exposure(data.frame(mean = 0, sd = 0),
                            mean = "mean",
                            sd = "sd",
                            n = 1)
  
  # Output is a matrix, so type == "double" with 2 dimensions
  expect_type(out, "double")
  expect_equal(dim(out), c(1, 1))
  expect_true(all(out == 0))
  
  out <- .simulate_exposure(data.frame(mean = 1, sd = NA),
                            mean = "mean",
                            sd = "sd",
                            n = 1)
  
  # Output is a matrix, so type == "double" with 2 dimensions
  expect_type(out, "double")
  expect_equal(dim(out), c(1, 1))
  expect_true(all(out == 1))
  
})
