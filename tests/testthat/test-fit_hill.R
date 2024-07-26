test_that("output column names", {
  df <- data.frame(
    logc = -3:3,
    resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
  )
  colnames <- c("tp", "tp.sd", "logAC50", "logAC50.sd", "slope", "slope.sd",
                "logc_min", "logc_max", "resp_min", "resp_max",
                "AIC", "tp.sd.imputed", "logAC50.sd.imputed")
  
  expect_equal(names(fit_hill(df)),
               colnames)
  
  df <- rbind(df, df)
  df$chem_col <- rep(c("A", "B"), each = 21)
  
  expect_equal(names(fit_hill(df, chem = "chem_col")),
               c("chem", colnames))

  df <- rbind(df, df)
  df$assay_col <- rep(c("a", "b"), each = 42)
  
  expect_equal(names(fit_hill(df, chem = "chem_col", assay = "assay_col")),
               c("assay", "chem", colnames))
})
