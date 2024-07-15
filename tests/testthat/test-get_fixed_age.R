test_that("expected results", {
  x <- data.frame(age_min        = seq(0, 50, 10),
                  age_median_css = seq(1, 51, 10))
  x <- x[nrow(x):1, ] # Mix up order
  y <- data.frame(age_min        = seq(0, 50, 10),
                  age_median_css = seq(2, 52, 10))
  simulated_css <- list("chem1" = x, "chem2" = y)
  
  expect_equal(
    get_fixed_age(simulated_css, c(17, 27, 37)),
    list(matrix(c(11, 21, 31, 12, 22, 32),
                ncol = 2,
                dimnames = list(NULL, c("chem1", "chem2"))))
  )
})
