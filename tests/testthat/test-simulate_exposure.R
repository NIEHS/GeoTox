test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    simulate_exposure(GT),
    "No 'exposure' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "exposure", data.frame(a = 1))
  expect_error(
    simulate_exposure(GT, sensitivity = TRUE),
    "No 'concentration_sensitivity' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "concentration_sensitivity", data.frame(a = 1))
  expect_error(
    simulate_exposure(GT, sensitivity = TRUE),
    "No 'sample' table found in the GeoTox connection."
  )
})

test_that("sample table already exists", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup needed tables
  sample_df <- tibble::tibble(
    FIPS = c(rep("00001", 2), rep("00002", 3))
  )
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mean, ~sd,
    "00001", "50-00-0", "inhalation",  0, 2,
    "00001", "64-17-5", "inhalation", 20, 5,
    "00002", "50-00-0", "inhalation", 15, 3,
    "00002", "64-17-5", "inhalation", 25, 4,
    "00003", "50-00-0", "inhalation", 12, 2
  )
  GT |> set_sample(sample_df) |> add_exposure(exposure_df)

  # No concentration table
  expect_silent(simulate_exposure(GT))

  # Overwrite
  expect_error(
    simulate_exposure(GT),
    "GeoTox connection already has a 'concentration' table with a 'C_ext' column."
  )

  # Concentration table w/o C_ext
  DBI::dbExecute(con, "ALTER TABLE concentration DROP COLUMN C_ext")
  expect_silent(simulate_exposure(GT))

  # Output
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(nrow(conc_tbl), 10)
  expect_true("C_ext" %in% colnames(conc_tbl))

  # Sensitivity
  conc_sens_tbl <- dplyr::tbl(con, "concentration") |>
    dplyr::select(id, sample_id, substance_id, route_id, C_ext) |>
    dplyr::compute(
      name = "concentration_sensitivity",
      overwrite = TRUE,
      temporary = FALSE
    )
  expect_silent(simulate_exposure(GT, sensitivity = TRUE))
})

test_that("sample table doesn't exist", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup needed tables
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mean, ~sd,
    "00001", "50-00-0", "inhalation",  0, 2,
    "00001", "64-17-5", "inhalation", 20, 5,
    "00002", "50-00-0", "inhalation", 15, 3,
    "00002", "64-17-5", "inhalation", 25, 4,
    "00003", "50-00-0", "inhalation", 12, 2
  )
  GT |> add_exposure(exposure_df)

  # Simulate
  expect_silent(simulate_exposure(GT, n = 4))

  # Output
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(nrow(conc_tbl), 24) # 3 locations * 4 samples * 2 substances
  expect_true("C_ext" %in% colnames(conc_tbl))
  sample_tbl <- dplyr::tbl(con, "sample") |> dplyr::collect()
  expect_equal(nrow(sample_tbl), 12) # 3 locations * 4 samples
})

test_that("other column names", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup needed tables
  sample_df <- tibble::tibble(
    FIPS = c(rep("00001", 2), rep("00002", 3))
  )
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mu, ~sigma,
    "00001", "50-00-0", "inhalation", 10, 2,
    "00001", "64-17-5", "inhalation", 20, 5,
    "00002", "50-00-0", "inhalation", 15, 3,
    "00002", "64-17-5", "inhalation", 25, 4,
    "00003", "50-00-0", "inhalation", 12, 2
  )
  GT |> set_sample(sample_df) |> add_exposure(exposure_df)

  # Input column names
  expect_silent(
    GT <- simulate_exposure(GT, expos_mean = "mu", expos_sd = "sigma")
  )

  # Using stored GT$par values
  expect_silent(
    simulate_exposure(GT, overwrite = TRUE)
  )
})

test_that("data order", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup needed tables
  exposure_df_1 <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mean, ~sd,
    "00001", "50-00-0", "inhalation",  0, 0,
    "00001", "64-17-5", "inhalation", 20, 0
  )
  # Reverse substance order for this location
  exposure_df_2 <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mean, ~sd,
    "00002", "64-17-5", "inhalation", 25, 0,
    "00002", "50-00-0", "inhalation", 15, 0
  )
  GT |> add_exposure(exposure_df_1) |> add_exposure(exposure_df_2)

  # Simulate
  expect_silent(simulate_exposure(GT, n = 1))

  # Output
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(nrow(conc_tbl), 4) # 2 locations * 2 substances
  expect_true("C_ext" %in% colnames(conc_tbl))
  sample_tbl <- dplyr::tbl(con, "sample") |> dplyr::collect()
  expect_equal(nrow(sample_tbl), 2) # 2 locations * 1 sample
  # Check order of C_ext values, should be ordered by location then substance
  expect_equal(
    conc_tbl |> dplyr::arrange(id) |> dplyr::pull(C_ext),
    c(0, 20, 15, 25)
  )
})
