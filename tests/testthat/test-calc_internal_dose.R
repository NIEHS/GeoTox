test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    calc_internal_dose(GT),
    "No 'concentration' table found in the GeoTox connection."
  )
  expect_error(
    calc_internal_dose(GT, sensitivity = TRUE),
    "No 'concentration_sensitivity' table found in the GeoTox connection."
  )
  df <- tibble::tribble(
    ~id, ~sample_id, ~route_id, ~C_ext,
    1, 1, 1, 0.5
  )
  write_table(con, "concentration", df)
  write_table(con, "concentration_sensitivity", df)
  expect_error(
    calc_internal_dose(GT),
    "No 'exposure_rate' table found in the GeoTox connection."
  )
  expect_error(
    calc_internal_dose(GT, sensitivity = TRUE),
    "No 'exposure_rate_sensitivity' table found in the GeoTox connection."
  )
})

test_that("calculate internal dose", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Don't match rows to make sure join is working
  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~route_id, ~C_ext,
    1, 1, 1, 0.5,
    2, 1, 2, 0.3,
    3, 2, 1, 0.2
  )
  rate_df <- tibble::tribble(
    ~sample_id, ~route_id, ~rate,
    2, 1, 20,
    1, 2, 5,
    1, 1, 10
  )
  write_table(con, "concentration", conc_df)
  write_table(con, "exposure_rate", rate_df)

  # Call function
  calc_internal_dose(GT)

  # Check results
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(conc_tbl$D_int, c(0.5 * 10, 0.3 * 5, 0.2 * 20))

  # Overwrite error
  expect_error(
    calc_internal_dose(GT),
    "GeoTox connection already has a 'D_int' column in the 'concentration' table."
  )

  # Modify input data and overwrite results
  sql <- "UPDATE concentration SET C_ext = C_ext + 1"
  invisible(DBI::dbExecute(con, sql))
  calc_internal_dose(GT, overwrite = TRUE)

  # Check results
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(conc_tbl$D_int, c(1.5 * 10, 1.3 * 5, 1.2 * 20))
})
