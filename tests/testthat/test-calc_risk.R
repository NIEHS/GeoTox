test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    calc_risk(GT),
    "No 'concentration' table found in the GeoTox connection."
  )
  expect_error(
    calc_risk(GT, risk_name = "risk_sensitivity"),
    "No 'concentration_sensitivity' table found in the GeoTox connection."
  )
  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~C_invitro,
    1, 1, 1, 0.5,
    2, 1, 2, 0.3
  )
  write_table(con, "concentration", conc_df)
  expect_error(
    calc_risk(GT),
    "No 'sample' table found in the GeoTox connection."
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 101,
    2, 102
  )
  write_table(con, "sample", sample_df)
  expect_error(
    calc_risk(GT),
    "No 'hill_params' table found in the GeoTox connection."
  )
})

test_that("no assay_id, reset_seed, extra loc", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"), reset_seed = TRUE)
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~C_invitro,
    1, 1, 1, 0.5,
    2, 1, 2, 0.3,
    3, 2, 1, 0.4,
    4, 2, 2, 0.6
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 1,
    2, 1
  )
  location_df <- tibble::tribble(
    ~id, ~FIPS,
    1, "00001",
    2, "00002"
  )
  hill_df <- tibble::tribble(
    ~substance_id,
    ~tp, ~tp.sd, ~logAC50, ~logAC50.sd, ~logc_min, ~logc_max, ~resp_max,
    1, 0.5, 0.1, -5, 0.2, -8, -2, 1.0,
    2, 0.6, 0.1, -6, 0.2, -9, -3, 1.0
  )
  write_table(con, "concentration", conc_df)
  write_table(con, "sample", sample_df)
  write_table(con, "location", location_df)
  write_table(con, "hill_params", hill_df)

  GT <- calc_risk(GT)

  risk_tbl <- dplyr::tbl(con, "risk") |> dplyr::collect()
  risk_cols <- c(
    "assay_id", "sample_id", "GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"
  )
  expect_true(all(risk_cols %in% colnames(risk_tbl)))
  expect_length(risk_tbl$assay_id, 2)
  expect_true(all(is.na(risk_tbl$assay_id)))
})

test_that("with assay_id, extra hill", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~C_invitro,
    1, 1, 1, 0.5,
    2, 1, 2, 0.3,
    3, 2, 1, 0.4,
    4, 2, 2, 0.6
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
    ~assay_id, ~substance_id,
    ~tp, ~tp.sd, ~logAC50, ~logAC50.sd, ~logc_min, ~logc_max, ~resp_max,
    1, 1, 0.5, 0.1, -5, 0.2, -8, -2, 1.0,
    2, 2, -0.6, 0.1, -6, 0.2, -9, -3, 1.0,
    3, 3, 0.7, 0.1, -7, 0.2, -10, -4, 1.0
  )
  write_table(con, "concentration", conc_df)
  write_table(con, "sample", sample_df)
  write_table(con, "location", location_df)
  write_table(con, "hill_params", hill_df)

  GT <- calc_risk(GT)

  risk_tbl <- dplyr::tbl(con, "risk") |> dplyr::collect()
  risk_cols <- c(
    "assay_id", "sample_id", "GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"
  )
  expect_true(all(risk_cols %in% colnames(risk_tbl)))
  expect_length(risk_tbl$assay_id, 6)
  expect_equal(sum(is.na(risk_tbl$GCA.Eff)), 2)
  expect_true(all(!is.na(risk_tbl$assay_id)))
})

test_that("overwrite", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  conc_df <- tibble::tribble(
    ~id, ~sample_id, ~substance_id, ~C_invitro,
    1, 1, 1, 0.5
  )
  sample_df <- tibble::tribble(
    ~id, ~location_id,
    1, 1
  )
  location_df <- tibble::tribble(
    ~id, ~FIPS,
    1, "00001"
  )
  hill_df <- tibble::tribble(
    ~substance_id,
    ~tp, ~tp.sd, ~logAC50, ~logAC50.sd, ~logc_min, ~logc_max, ~resp_max,
    1, 0.5, 0.1, -5, 0.2, -8, -2, 1.0
  )
  write_table(con, "concentration", conc_df)
  write_table(con, "sample", sample_df)
  write_table(con, "location", location_df)
  write_table(con, "hill_params", hill_df)

  GT <- calc_risk(GT)

  expect_error(
    calc_risk(GT),
    "GeoTox connection already has a 'risk' table."
  )

  set.seed(1234)
  GT <- calc_risk(GT, overwrite = TRUE)

  risk_tbl <- dplyr::tbl(con, "risk") |> dplyr::collect()
  expect_equal(nrow(risk_tbl), 1)

  expect_snapshot(risk_tbl)
})
