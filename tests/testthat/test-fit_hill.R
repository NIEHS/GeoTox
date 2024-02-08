test_that("output column names", {
  expect_equal(
    {
      df <- data.frame(
        logc = -3:3,
        resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
      )
      names(fit_hill(df))
    },
    c(
      "tp", "tp.sd", "logAC50", "logAC50.sd", "slope", "slope.sd",
      "logc_min", "logc_max", "resp_min", "resp_max",
      "AIC", "tp.sd.imputed", "logAC50.sd.imputed"
    )
  )
})
