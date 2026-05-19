test_that("bad inputs", {
  # Input should be a data frame
  expect_error(
    fit_hill(c()),
    "`x` must be a data.frame"
  )
  # Expected required column names
  expect_error(
    fit_hill(data.frame(x = 0, y = 0)),
    "`x` must contain columns named by 'conc' and 'resp' inputs"
  )
  # Expected optional column names
  df <- data.frame(logc = c(-1, 1), resp = c(100, 0))
  expect_error(
    fit_hill(df, substance = "missing"),
    "`x` must contain column\\(s\\) named by 'substance' when not NULL"
  )
  expect_error(
    fit_hill(df, assay = "missing"),
    "`x` must contain column\\(s\\) named by 'assay' when not NULL"
  )
})

test_that("output column names", {
  df <- data.frame(
    logc = -3:3,
    resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
  )
  colnames <- c("tp", "tp.sd", "logAC50", "logAC50.sd", "slope", "slope.sd",
                "logc_min", "logc_max", "resp_min", "resp_max",
                "AIC", "tp.sd.imputed", "logAC50.sd.imputed")

  out <- fit_hill(df)
  expect_equal(names(out$fit), colnames)
  expect_null(out$substance)
  expect_null(out$assay)

  df <- rbind(df, df)
  df$substance_col <- rep(c("A", "B"), each = 21)

  out <- fit_hill(df, substance = "substance_col")
  expect_equal(names(out$fit), c("substance_col", colnames))
  expect_equal(out$substance, "substance_col")
  expect_null(out$assay)

  df <- rbind(df, df)
  df$assay_col <- rep(c("a", "b"), each = 42)

  out <- fit_hill(df, substance = "substance_col", assay = "assay_col")
  expect_equal(names(out$fit), c("assay_col", "substance_col", colnames))
  expect_equal(out$substance, "substance_col")
  expect_equal(out$assay, "assay_col")
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

  expect_equal(names(res), colnames)
})
