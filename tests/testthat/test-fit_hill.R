test_that("bad inputs", {
  # Input should be a data frame
  expect_error(fit_hill(c()))
  # Expected required column names
  expect_error(fit_hill(data.frame(x = 0, y = 0)))
  # Expected optional column names
  expect_error(fit_hill(data.frame(logc = c(-1, 1),
                                   resp = c(100, 0)),
                        chem = "missing"))
  expect_error(fit_hill(data.frame(logc = c(-1, 1),
                                   resp = c(100, 0)),
                        assay = "missing"))
})

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

test_that("internal", {
  
  x <- data.frame(logc = c(-1, 0, 1), resp = c(10, 5, 0))
  
  out <- .fit_hill(x)
  
  expect_equal(as.numeric(out$par["slope"]),
               1)
  
  out <- .fit_hill(x, fixed_slope = FALSE)
  
  expect_true(as.numeric(out$par["slope"]) > 1)
  
  res <- .extract_hill_params(out)
  
  colnames <- c("tp", "tp.sd", "logAC50", "logAC50.sd", "slope", "slope.sd",
                "logc_min", "logc_max", "resp_min", "resp_max",
                "AIC", "tp.sd.imputed", "logAC50.sd.imputed")
  
  expect_equal(names(res),
               colnames)
})
