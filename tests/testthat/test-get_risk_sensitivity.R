test_that("get risk sensitivity", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  risk_df <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(10, NA_real_)
  )
  risk_sensitivity_C_ext <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(20)
  )
  risk_sensitivity_css_params <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(30, 40)
  )
  risk_sensitivity_weight <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(50, 60)
  )
  risk_sensitivity_age <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(80)
  )
  risk_sensitivity_fit_params <- data.frame(
    sample_id = c(1, 2),
    GCA.Eff = c(90)
  )
  DBI::dbWriteTable(con, "risk", risk_df)
  DBI::dbWriteTable(con, "risk_sensitivity_C_ext", risk_sensitivity_C_ext)
  DBI::dbWriteTable(con, "risk_sensitivity_css_params", risk_sensitivity_css_params)
  DBI::dbWriteTable(con, "risk_sensitivity_weight", risk_sensitivity_weight)
  DBI::dbWriteTable(con, "risk_sensitivity_age", risk_sensitivity_age)
  DBI::dbWriteTable(con, "risk_sensitivity_fit_params", risk_sensitivity_fit_params)

  out <- get_risk_sensitivity(GT)
  expect_equal(
    out,
    tibble::tribble(
      ~C_ext, ~css_params, ~weight, ~age, ~fit_params, ~baseline,
      20, 30, 50, 80, 90, 10,
      20, 40, 60, 80, 90, NA_real_
    )
  )
})
