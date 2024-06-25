test_that("bad inputs", {
  # Input should be a data frame
  expect_error(simulate_obesity(c()))
  # Need columns named by "obes_prev" and "obes_sd"
  expect_error(simulate_obesity(data.frame(x = 0, y = 0)))
  # Need column named by "obes_label" if nrows > 1
  expect_error(simulate_obesity(data.frame(OBESITY_CrudePrev = c(0, 0),
                                           OBESITY_SD = c(0, 0))))
})

test_that("expected output", {
  
  x <- data.frame(prev = c(0, 100),
                  sd = c(0, 0),
                  label = c(1, 2))
  obesity <- simulate_obesity(x,
                              obes_prev = "prev",
                              obes_sd = "sd",
                              obes_label = "label",
                              n = 5)
  
  expect_length(obesity, nrow(x))
  expect_true(all(obesity[[1]] == "Normal"))
  expect_true(all(obesity[[2]] == "Obese"))
  expect_true(all(names(obesity) == c("1", "2")))
  
})
