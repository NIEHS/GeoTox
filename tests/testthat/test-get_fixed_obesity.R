test_that("expected results", {
  x <- data.frame(weight            = c("Normal", "Obese"),
                  weight_median_css = c(0, 1))
  y <- data.frame(weight            = c("Obese", "Normal"),
                  weight_median_css = c(3, 2))
  simulated_css <- list("chem1" = x, "chem2" = y)
  
  expect_equal(
    get_fixed_obesity(simulated_css, c("Normal", "Obese")),
    list(matrix(0:3,
                ncol = 2,
                dimnames = list(NULL, c("chem1", "chem2"))))
  )
})
