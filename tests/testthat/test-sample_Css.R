test_that("bad inputs", {
  # Missing age/obesity data
  expect_error(sample_Css(age = c(), obesity = c()))
  expect_error(sample_Css(age = NULL, obesity = NULL))
  # Age/obesity data do not match lengths
  expect_error(sample_Css(age = 1:3, obesity = c("Normal", "Obese")))
  # Age/obesity data do not match names
  expect_error(sample_Css(age = list("a" = 1:2),
                          obesity = list("b" = c("Normal", "Obese"))))
})

test_that("expected results", {
  
  x <- y <- expand.grid(age_min = seq(0, 50, 10), weight = c("Normal", "Obese"))
  x$css <- lapply(1:nrow(x), function(i) {
    rep(x$age_min[i] / 5 + as.integer(x$weight[i] == "Obese"), 2)
  })
  y$css <- lapply(1:nrow(y), function(i) {
    rep(y$age_min[i] / 10 + as.integer(y$weight[i] == "Obese") / 2, 2)
  })
  simulated_css <- list("chem1" = x, "chem2" = y)

  expect_equal(
    sample_Css(simulated_css,
               c(15, 25, 35),
               c("Normal", "Obese", "Normal")),
    list(matrix(c(2, 5, 6, 1, 2.5, 3),
                ncol = 2,
                dimnames = list(NULL, c("chem1", "chem2"))))
  )
})
