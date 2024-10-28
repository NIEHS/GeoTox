test_that("results", {
  expect_no_error(.check_types(matrix(1), "matrix"))
  expect_no_error(.check_types(list(matrix(1)), "matrix"))
  
  expect_error(.check_types(1, "matrix"))
  expect_error(.check_types(list(1), "matrix"))
})
