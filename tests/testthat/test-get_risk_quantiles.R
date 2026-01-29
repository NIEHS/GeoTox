test_that("get risk quantiles", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  risk_df <- tibble::tribble(
    ~assay_id, ~sample_id, ~GCA.Eff,
    1, 1, 10,
    1, 2, NA_real_,
    1, 3, 30,
    1, 4, 40,
    2, 2, 60
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 1,
    2, 1,
    3, 1,
    4, 2
  )

  # Missing tables
  expect_error(
    get_risk_quantiles(GT),
    "No 'risk' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "risk", risk_df)
  expect_error(
    get_risk_quantiles(GT),
    "No 'sample' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "sample", sample_df)

  # Get output
  out <- get_risk_quantiles(GT, quantiles = c(0.1, 0.5))

  # Compare output
  expect_equal(
    out,
    tibble::tribble(
      ~assay_id, ~location_id, ~quantile, ~value,
      1, 1, 0.1, 12,
      1, 1, 0.5, 20,
      1, 2, 0.1, 40,
      1, 2, 0.5, 40,
      2, 1, 0.1, 60,
      2, 1, 0.5, 60,
      2, 2, 0.1, NA_real_,
      2, 2, 0.5, NA_real_
    )
  )
})
