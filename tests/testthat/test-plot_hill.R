test_that("missing input", {
  expect_error(plot_hill(NULL),
               "No Hill parameters found")
})

test_that("multiple assays, multiple chemicals", {
  df <- data.frame(assay = rep(c("a1", "a2"), each = 2),
                   chem = rep(c("c1", "c2"), 2),
                   tp = c(1, 2, 3, 4),
                   logAC50 = c(-1, 0, 1, 2),
                   slope = c(4, 3, 2, 1))
  expect_no_error(plot_hill(df))
})

test_that("single assay, multiple chemicals", {
  df <- data.frame(chem = c("c1", "c2"),
                   tp = c(1, 2),
                   logAC50 = c(-1, 1),
                   slope = c(2, 1))
  expect_no_error(plot_hill(df))
})

test_that("single assay, single chemical", {
  df <- data.frame(tp = 1,
                   logAC50 = 0,
                   slope = 1.5)
  expect_no_error(plot_hill(df))
})
