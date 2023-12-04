test_that("output column names", {
  expect_equal(
    {
      df <- data.frame(
        logc = -3:3,
        resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
      )
      fit <- fit_hill(df$logc, df$resp)
      names(extract_hill_params(list(fit)))
    },
    c(
      "tp", "tp.sd", "logAC50", "logAC50.sd",
      "logc_min", "logc_max", "resp_min", "resp_max",
      "AIC", "tp.sd.imputed", "logAC50.sd.imputed"
    )
  )
})
