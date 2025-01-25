test_that("expected results", {
  x <- y <- expand.grid(age_min = seq(0, 50, 10), weight = c("Normal", "Obese"))
  x$css <- lapply(1:nrow(x), function(i) {
    rep(x$age_min[i] / 5 + as.integer(x$weight[i] == "Obese"), 2)
  })
  x <- x[nrow(x):1, ] # Mix up order
  y$css <- lapply(1:nrow(y), function(i) {
    rep(y$age_min[i] / 10 + as.integer(y$weight[i] == "Obese") / 2, 2)
  })
  simulated_css <- list("chem1" = x, "chem2" = y)
  
  # Age list
  out <- get_fixed_params(simulated_css, list(c(1, 27, 30), c(15, 60)))
  
  expect_equal(out,
               list(matrix(c(4, 4, 4, 2, 2, 2),
                           ncol = 2,
                           dimnames = list(NULL, c("chem1", "chem2"))),
                    matrix(c(6, 6, 3, 3),
                           ncol = 2,
                           dimnames = list(NULL, c("chem1", "chem2")))))
  
  # Age vector
  out <- get_fixed_params(simulated_css, c(1, 27, 30))
  
  expect_equal(out,
               list(matrix(c(4, 4, 4, 2, 2, 2),
                           ncol = 2,
                           dimnames = list(NULL, c("chem1", "chem2")))))
})
