test_that("sensitivity analysis", {
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
    sensitivity_analysis(GT)
  )

  # Check results
  tables <- c(
    "risk_sensitivity_age",
    "risk_sensitivity_weight",
    "risk_sensitivity_css_params",
    "risk_sensitivity_fit_params",
    "risk_sensitivity_C_ext"
  )
  res <- tables |>
    purrr::map(\(x) dplyr::tbl(con, x) |> dplyr::collect()) |>
    rlang::set_names(tables)
  expect_snapshot(res)
})
