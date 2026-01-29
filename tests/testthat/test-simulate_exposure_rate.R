test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    simulate_exposure_rate(GT),
    "No 'sample' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "sample", data.frame(id = 1, location_id = 1, age = 30))

  expect_error(
    simulate_exposure_rate(GT),
    "No 'location' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "location", data.frame(id = 1, FIPS = "00001"))

  expect_error(
    simulate_exposure_rate(GT),
    "No 'route' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "route", data.frame(id = 1, name = "inhalation"))

  expect_error(
    simulate_exposure_rate(GT),
    "No 'exposure_rate_params' table found in the GeoTox connection."
  )

  expect_error(
    simulate_exposure_rate(GT, sensitivity = TRUE),
    "No 'exposure_rate_sensitivity' table found in the GeoTox connection."
  )

})

test_that("custom params", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~gender, ~other,
    "37003", 25, "female", "other",
    "37003", 35, "male",   "other",
    "37005", 45, "female", "other"
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

  GT <- GT |>
    set_sample(sample_df) |>
    add_exposure_rate_params(route = "custom", params = params) |>
    simulate_exposure_rate(rate_extra_cols = c("gender", "other"))

  expect_equal(GT$par$rate_extra, c("gender", "other"))

  # Using stored GT$par
  simulate_exposure_rate(GT, overwrite = TRUE)

  rate_tbl <- dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
  expect_equal(rate_tbl$rate[1:2], c(25, 1))
})

test_that("simulate exposure rate", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  sample_df <- tibble::tribble(
    ~FIPS, ~age,
    "37003", 25,
    "37003", 35,
    "37005", 45
  )

  GT |>
    set_sample(sample_df) |>
    add_location(data.frame(FIPS = "37007")) |> # extra location
    add_exposure_rate_params(route = "inhalation") |>
    simulate_exposure_rate()

  rate_tbl <- dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
  expect_equal(nrow(rate_tbl), 3)
  expect_true(all(!is.na(rate_tbl$rate)))

  # Overwrite
  expect_error(
    simulate_exposure_rate(GT),
    "GeoTox connection already has an 'exposure_rate' table."
  )

  # Sensitivity
  rate_sens_tbl <- dplyr::tbl(con, "exposure_rate") |>
    dplyr::compute(
      name = "exposure_rate_sensitivity",
      temporary = FALSE
    )
  simulate_exposure_rate(GT, sensitivity = TRUE)
  rate_sens_tbl <- dplyr::tbl(con, "exposure_rate_sensitivity") |> dplyr::collect()
  expect_equal(nrow(rate_tbl), 3)
  expect_true(all(!is.na(rate_sens_tbl$rate)))
})
