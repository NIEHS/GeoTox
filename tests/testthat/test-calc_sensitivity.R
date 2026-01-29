test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    calc_sensitivity(GT),
    "No 'exposure_rate' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "exposure_rate", data.frame(id = 1))

  expect_error(
    calc_sensitivity(GT),
    "No 'concentration' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "concentration", data.frame(id = 1))

  expect_error(
    calc_sensitivity(GT),
    "No 'fixed_css' table found in the GeoTox connection."
  )
})

test_that("calc sensitivity", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  set.seed(1234)

  # Setup tables
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight,
    "00001", 25, "Normal",
    "00001", 35, "Normal",
    "00002", 45, "Obese",
  )
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mean, ~sd,
    "00001", "50-00-0", "inhalation", 10, 2,
    "00001", "50-00-1", "inhalation", 20, 5,
    "00002", "50-00-0", "inhalation", 15, 3,
    "00002", "50-00-1", "inhalation", 25, 4
  )
  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight, ~css,
    "50-00-0",  0, 44, "Normal", 1,
    "50-00-0", 45, 89, "Normal", 2,
    "50-00-0",  0, 44, "Obese", 11,
    "50-00-0", 45, 89, "Obese", 12,
    "50-00-1",  0, 44, "Normal", 3,
    "50-00-1", 45, 89, "Normal", 4,
    "50-00-1",  0, 44, "Obese", 13,
    "50-00-1", 45, 89, "Obese", 14
  )
  hill_df <- rbind(
    data.frame(
      casn = "50-00-0",
      logc = -3:3,
      resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
    ),
    data.frame(
      casn = "50-00-01",
      logc = -3:3,
      resp = 10 / (1 + 10^(0.8 * (1.4 - rep(-3:3, each = 3)))) + rnorm(21)
    )
  )
  hill_fit <- fit_hill(hill_df, substance = "casn")
  GT <- GT |>
    set_sample(sample_df) |>
    set_simulated_css(css_df) |>
    add_exposure_rate_params() |>
    add_exposure(exposure_df) |>
    add_hill_params(hill_fit) |>
    simulate_exposure_rate() |>
    simulate_exposure() |>
    sample_simulated_css() |>
    set_fixed_css()

  expect_silent(
    calc_sensitivity(GT)
  )

  # Overwrite
  expect_error(
    calc_sensitivity(GT),
    "GeoTox connection already has a 'risk_sensitivity_age' table."
  )
  calc_sensitivity(GT, overwrite = TRUE)

  # Check results
  sens_tbl <- dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
  expect_snapshot(sens_tbl)
})

test_that("other inputs", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  set.seed(1234)

  # Setup tables
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight, ~gender, ~other,
    "00001", 25, "Normal", "female", "other",
    "00001", 35, "Normal", "male",   "other",
    "00002", 45, "Obese",  "female", "other"
  )
  params <- tibble::tribble(
    ~age_lb, ~age_ub, ~gender, ~other, ~mean, ~sd,
    20, 30, "male",   "other",  0.1, 2,
    30, 40, "male",   "other",  1,   0,
    40, 50, "male",   "other", 10,   2,
    20, 30, "female", "other", 25,   0,
    30, 40, "female", "other", 50,   3,
    40, 50, "female", "other", 75,   3
  )
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~mu, ~sigma,
    "00001", "50-00-0", "custom", 10, 2,
    "00001", "50-00-1", "custom", 20, 5,
    "00002", "50-00-0", "custom", 15, 3,
    "00002", "50-00-1", "custom", 25, 4
  )
  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight, ~css,
    "50-00-0",  0, 44, "Normal", 1,
    "50-00-0", 45, 89, "Normal", 2,
    "50-00-0",  0, 44, "Obese", 11,
    "50-00-0", 45, 89, "Obese", 12,
    "50-00-1",  0, 44, "Normal", 3,
    "50-00-1", 45, 89, "Normal", 4,
    "50-00-1",  0, 44, "Obese", 13,
    "50-00-1", 45, 89, "Obese", 14
  )
  hill_df <- rbind(
    data.frame(
      casn = "50-00-0",
      logc = -3:3,
      resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
    ),
    data.frame(
      casn = "50-00-01",
      logc = -3:3,
      resp = 10 / (1 + 10^(0.8 * (1.4 - rep(-3:3, each = 3)))) + rnorm(21)
    )
  )
  hill_fit <- fit_hill(hill_df, substance = "casn")
  GT <- GT |>
    set_sample(sample_df) |>
    set_simulated_css(css_df) |>
    add_exposure_rate_params(route = "custom", params = params) |>
    add_exposure(exposure_df) |>
    add_hill_params(hill_fit) |>
    simulate_exposure_rate(rate_extra_cols = c("gender", "other")) |>
    simulate_exposure(expos_mean = "mu", expos_sd = "sigma") |>
    sample_simulated_css() |>
    set_fixed_css()

  expect_silent(
    calc_sensitivity(GT, vary = "C_ext")
  )

  # Check results
  sens_tbl <- dplyr::tbl(con, "risk_sensitivity_C_ext") |> dplyr::collect()
  expect_snapshot(sens_tbl)
})
