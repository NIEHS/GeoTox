test_that("get concentration mean", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  conc_df <- tibble::tribble(
    ~substance_id, ~route_id, ~sample_id, ~C_ext,
    1, 1, 1, 10,
    1, 1, 2, NA_real_,
    1, 1, 3, 30,
    1, 1, 4, 40,
    2, 1, 2, 60
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
    get_concentration_mean(GT, "C_ext"),
    "No 'concentration' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "concentration", conc_df)
  expect_error(
    get_concentration_mean(GT, "C_ext"),
    "No 'sample' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "sample", sample_df)

  # Get output
  out <- get_concentration_mean(GT, "C_ext")

  # Compare output
  expect_equal(
    out,
    tibble::tribble(
      ~substance_id, ~route_id, ~location_id, ~mean,
      1, 1, 1, 20,
      1, 1, 2, 40,
      2, 1, 1, 60,
      2, 1, 2, NA_real_
    )
  )
})
