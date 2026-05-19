test_that("missing table", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    calc_invitro_concentration(GT),
    "No 'concentration' table found in the GeoTox connection."
  )
  expect_error(
    calc_invitro_concentration(GT, sensitivity = TRUE),
    "No 'concentration_sensitivity' table found in the GeoTox connection."
  )
})

test_that("calculate in vitro concentration", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~route_id, ~C_ss, ~D_int,
    1, 1, 1, 0.5, 2,
    2, 1, 2, 0.3, 3,
    3, 2, 1, 0.2, 4
  )
  write_table(con, "concentration", conc_df)

  # Call function
  GT <- calc_invitro_concentration(GT)

  # Check results
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(conc_tbl$C_invitro, conc_df$C_ss * conc_df$D_int)

  # Overwrite error
  expect_error(
    calc_invitro_concentration(GT),
    "GeoTox connection already has a 'C_invitro' column in the 'concentration' table. Set `overwrite = TRUE` to replace the existing values.\nWarning: Overwriting will not alter downstream values."
  )

  # Modify input data and overwrite results
  sql <- "UPDATE concentration SET C_ss = C_ss + 1"
  invisible(DBI::dbExecute(con, sql))
  GT <- calc_invitro_concentration(GT, overwrite = TRUE)

  # Check results
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(conc_tbl$C_invitro, (conc_df$C_ss + 1) * conc_df$D_int)
})
