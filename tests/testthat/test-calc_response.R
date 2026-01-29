test_that("normal risk", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  risk_name <- "risk"
  conc_name <- "concentration"
  rate_name <- "exposure_rate"

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~route_id, ~C_ext, ~C_ss,
    1, 1, 1, 1, 0.5, 0.7,
    2, 1, 2, 2, 0.3, 0.5,
    3, 2, 1, 1, 0.4, 0.6,
    4, 2, 2, 2, 0.6, 0.8
  )
  rate_df <- tibble::tribble(
    ~sample_id, ~route_id, ~rate,
    1, 1, 5,
    1, 2, 10,
    2, 1, 15,
    2, 2, 20
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 1,
    2, 1
  )
  location_df <- tibble::tribble(
    ~id, ~FIPS,
    1, "00001"
  )
  hill_df <- tibble::tribble(
    ~substance_id,
    ~tp, ~tp.sd, ~logAC50, ~logAC50.sd, ~logc_min, ~logc_max, ~resp_max,
    1, 0.5, 0.1, -5, 0.2, -8, -2, 1.0,
    2, 0.6, 0.1, -6, 0.2, -9, -3, 1.0
  )
  write_table(con, conc_name, conc_df)
  write_table(con, rate_name, rate_df)
  write_table(con, "sample", sample_df)
  write_table(con, "location", location_df)
  write_table(con, "hill_params", hill_df)

  set.seed(1234)
  GT <- GT |> calc_response()

  risk_tbl <- dplyr::tbl(con, risk_name) |> dplyr::collect()
  risk_cols <- c(
    "assay_id", "sample_id", "GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"
  )
  expect_true(all(risk_cols %in% colnames(risk_tbl)))

  expect_snapshot(risk_tbl)
})

test_that("risk sensitivity", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  risk_name <- "risk_sensitivity"
  conc_name <- "concentration_sensitivity"
  rate_name <- "exposure_rate_sensitivity"

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~route_id, ~C_ext, ~C_ss,
    1, 1, 1, 1, 0.5, 0.7,
    2, 1, 2, 2, 0.3, 0.5,
    3, 2, 1, 1, 0.4, 0.6,
    4, 2, 2, 2, 0.6, 0.8
  )
  rate_df <- tibble::tribble(
    ~sample_id, ~route_id, ~rate,
    1, 1, 5,
    1, 2, 10,
    2, 1, 15,
    2, 2, 20
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 1,
    2, 1
  )
  location_df <- tibble::tribble(
    ~id, ~FIPS,
    1, "00001"
  )
  hill_df <- tibble::tribble(
    ~substance_id,
    ~tp, ~tp.sd, ~logAC50, ~logAC50.sd, ~logc_min, ~logc_max, ~resp_max,
    1, 0.5, 0.1, -5, 0.2, -8, -2, 1.0,
    2, 0.6, 0.1, -6, 0.2, -9, -3, 1.0
  )
  write_table(con, conc_name, conc_df)
  write_table(con, rate_name, rate_df)
  write_table(con, "sample", sample_df)
  write_table(con, "location", location_df)
  write_table(con, "hill_params", hill_df)

  set.seed(1234)
  GT <- GT |> calc_response(risk_name = risk_name, max_mult = 2, fixed = TRUE)

  risk_tbl <- dplyr::tbl(con, risk_name) |> dplyr::collect()
  risk_cols <- c(
    "assay_id", "sample_id", "GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"
  )
  expect_true(all(risk_cols %in% colnames(risk_tbl)))

  expect_snapshot(risk_tbl)
})
