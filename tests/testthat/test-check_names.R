test_that("results", {
  # TRUE means names not found
  names <- "a"
  expect_true(.check_names(c(), names = names))
  expect_true(.check_names(list(c()), names = names))
  expect_true(.check_names(list(c("a" = 1),
                                c()), names = names))
  expect_false(.check_names(c("a" = 1), names = names))
  expect_false(.check_names(list(c("a" = 1)), names = names))
})
