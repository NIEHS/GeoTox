test_that("print", {
  # Extra input, "config", for duckdb driver
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"), config = list())
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Empty object
  expect_no_error(capture_output(print(GT)))

  # Range of sample sizes, no info for other tables
  sample_df <- data.frame(FIPS = c("00001", "00001", "00002"))
  conc_df <- data.frame(id = 1, sample_id = 1, substance_id = 1, route_id = 1)
  risk_df <- data.frame(assay_id = 1, sample_id = 1)
  GT |> set_sample(sample_df)
  DBI::dbWriteTable(con, "concentration", conc_df)
  DBI::dbWriteTable(con, "risk", risk_df)
  expect_no_error(capture_output(print(GT)))

  # Single sample size, info for other tables
  sample_df <- data.frame(FIPS = c("00001", "00002"))
  conc_df <- data.frame(
    id = 1, sample_id = 1, substance_id = 1, route_id = 1, C_ext = 1
  )
  risk_df <- data.frame(assay_id = 1, sample_id = 1, GCA.Eff = 1)
  GT |> set_sample(sample_df, overwrite = TRUE)
  DBI::dbWriteTable(con, "concentration", conc_df, overwrite = TRUE)
  DBI::dbWriteTable(con, "risk", risk_df, overwrite = TRUE)
  DBI::dbWriteTable(con, "risk_sensitivity_age", risk_df)
  expect_no_error(capture_output(print(GT)))
})
