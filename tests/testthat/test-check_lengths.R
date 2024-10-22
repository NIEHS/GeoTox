test_that("results", {
  # TRUE means different lengths
  expect_true(.check_lengths(list(1:5),
                             list(6:9)))
  expect_false(.check_lengths(list(1:5),
                              list(6:10)))
})
