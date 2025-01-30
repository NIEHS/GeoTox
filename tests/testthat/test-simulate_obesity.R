test_that("bad inputs", {
  # Input should be a data frame
  expect_error(simulate_obesity(c()),
               "`x` must be a data frame")
  # Need columns named by "obes_prev" and "obes_sd"
  expect_error(simulate_obesity(data.frame(x = 0, y = 0)),
               "`x` must contain columns named by `obes_prev` and `obes_sd`")
  # Need column named by "obes_label" if nrows > 1
  expect_error(simulate_obesity(data.frame(OBESITY_CrudePrev = c(0, 0),
                                           OBESITY_SD = c(0, 0))),
               "`x` must contain a column named by `obes_label`")
  # Length mismatch
  expect_error(simulate_obesity(data.frame(OBESITY_CrudePrev = c(0, 0),
                                           OBESITY_SD = c(0, 0),
                                           FIPS = letters[1:2]),
                                n = 1:3),
               paste("`n` must be a single value or a vector the same length",
                     "as `nrow\\(x\\)`"))
})

test_that("single row", {
  
  x <- data.frame(OBESITY_CrudePrev = 50,
                  OBESITY_SD = 5)
  
  out <- simulate_obesity(x, n = 5)
  
  expect_type(out, "list")
  expect_length(out, 1)
  expect_equal(names(out), NULL)
  expect_equal(length(out[[1]]), 5)
  expect_true(all(out[[1]] %in% c("Normal", "Obese")))

})

test_that("default column names", {
  
  x <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
                  OBESITY_SD = c(5, 5, 5),
                  FIPS = c("c", "a", "b"))
  
  out <- simulate_obesity(x, n = 5)
  
  expect_type(out, "list")
  expect_length(out, 3)
  expect_equal(names(out), x$FIPS)
  expect_equal(lapply(out, length), stats::setNames(list(5, 5, 5), x$FIPS))
  expect_true(all(unlist(out) %in% c("Normal", "Obese")))

})

test_that("custom column names", {
  
  x <- data.frame(prev = c(0, 50, 100),
                  sd = c(0, 5, 0),
                  label = letters[1:3])
  
  out <- simulate_obesity(x,
                          obes_prev = "prev",
                          obes_sd = "sd",
                          obes_label = "label",
                          n = 5)
  
  expect_type(out, "list")
  expect_length(out, 3)
  expect_equal(names(out), x$label)
  expect_equal(lapply(out, length), stats::setNames(list(5, 5, 5), x$label))
  expect_true(all(unlist(out) %in% c("Normal", "Obese")))
  expect_true(all(out[[1]] == "Normal"))
  expect_true(all(out[[3]] == "Obese"))
  
})

test_that("variable n", {
  
  x <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
                  OBESITY_SD = c(5, 5, 5),
                  FIPS = c("c", "a", "b"))
  
  out <- simulate_obesity(x, n = 5:3)
  
  expect_type(out, "list")
  expect_length(out, 3)
  expect_equal(names(out), x$FIPS)
  expect_equal(lapply(out, length), stats::setNames(list(5, 4, 3), x$FIPS))
  expect_true(all(unlist(out) %in% c("Normal", "Obese")))
  
})
