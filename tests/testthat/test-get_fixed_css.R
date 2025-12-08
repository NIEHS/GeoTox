test_that("errors", {
  expect_error(get_fixed_css(age = c(1, 2), obesity = c("Normal")))
})

test_that("results", {
  simulated_css <- list(
    "cas1" = tibble::tibble(age_min = 1,
                            age_median_css = 5,
                            weight = "Normal",
                            weight_median_css = 10,
                            css = list(c(15, 15)))
  )
  C_ss <- list(matrix(7))
  age <- 2
  obesity <- "Normal"

  out <- get_fixed_css(simulated_css = simulated_css,
                       C_ss = C_ss,
                       age = age,
                       obesity = obesity)

  expected <- list(
    age = list(matrix(5)),
    params = list(matrix(15)),
    obesity = list(matrix(10)),
    other = list(7)
  )
  dimnames(expected$age[[1]]) <- list(NULL, "cas1")
  dimnames(expected$params[[1]]) <- list(NULL, "cas1")
  dimnames(expected$obesity[[1]]) <- list(NULL, "cas1")
  expect_equal(out, expected)
})
