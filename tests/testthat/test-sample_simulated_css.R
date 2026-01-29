test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    sample_simulated_css(GT),
    "No 'simulated_css' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "simulated_css", data.frame(id = 1))
  expect_error(
    sample_simulated_css(GT),
    "No 'concentration' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "concentration", data.frame(id = 1))
  expect_error(
    sample_simulated_css(GT),
    "No 'sample' table found in the GeoTox connection."
  )
})

test_that("sample simulated css", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight,
    10000, 30, "Normal",
    10000, 50, "Normal",
    10000, 30,  "Obese",
    10000, 50,  "Obese"
  )
  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight, ~css,
    "00-00-1",  0, 49, "Normal",  1,
    "00-00-1",  0, 49, "Normal",  1, # a second copy to hit test case
    "00-00-1", 50, 99, "Normal",  2,
    "00-00-1",  0, 49,  "Obese", 11,
    "00-00-1", 50, 99,  "Obese", 12,
    "00-00-2",  0, 49, "Normal", 21,
    "00-00-2", 50, 99, "Normal", 22,
    "00-00-2",  0, 49,  "Obese", 31,
    "00-00-2", 50, 99,  "Obese", 32
  ) |>
    # Mix up order to test join, keep first row for substance order
    dplyr::arrange(c(1, sample(2:9)))
  GT |> set_simulated_css(css_df) |> set_sample(sample_df)
  conc_df <- dplyr::cross_join(
    dplyr::tbl(con, "sample") |> dplyr::select(sample_id = id),
    dplyr::tbl(con, "substance") |> dplyr::select(substance_id = id)
  ) |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::compute(name = "concentration", temporary = FALSE)

  sample_simulated_css(GT)

  conc_tbl <- dplyr::tbl(con, "concentration") |>
    dplyr::collect() |> dplyr::arrange(id)
  expect_equal(conc_tbl$C_ss, c(1, 2, 11, 12, 21, 22, 31, 32))
})

test_that("extra inputs", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight, ~sign, ~no_match,
    10000, 30, "Normal", "+", "x",
    10000, 50, "Normal", "-", "x",
    10000, 30,  "Obese", "+", "x",
    10000, 50,  "Obese", "-", "x"
  )
  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight, ~css, ~sign, ~no_match,
    "00-00-1",  0, 49, "Normal",   1, "+", "no_match",
    "00-00-1", 50, 99, "Normal",   2, "+", "no_match",
    "00-00-1",  0, 49, "Normal",  -1, "-", "no_match",
    "00-00-1", 50, 99, "Normal",  -2, "-", "no_match",
    "00-00-1",  0, 49,  "Obese",  11, "+", "no_match",
    "00-00-1", 50, 99,  "Obese",  12, "+", "no_match",
    "00-00-1",  0, 49,  "Obese", -11, "-", "no_match",
    "00-00-1", 50, 99,  "Obese", -12, "-", "no_match",
    "00-00-2",  0, 49, "Normal",  21, "+", "no_match",
    "00-00-2", 50, 99, "Normal",  22, "+", "no_match",
    "00-00-2",  0, 49, "Normal", -21, "-", "no_match",
    "00-00-2", 50, 99, "Normal", -22, "-", "no_match",
    "00-00-2",  0, 49,  "Obese",  31, "+", "no_match",
    "00-00-2", 50, 99,  "Obese",  32, "+", "no_match",
    "00-00-2",  0, 49,  "Obese", -31, "-", "no_match",
    "00-00-2", 50, 99,  "Obese", -32, "-", "no_match",
  )
  GT |> set_simulated_css(css_df) |> set_sample(sample_df)
  conc_df <- dplyr::cross_join(
    dplyr::tbl(con, "sample") |> dplyr::select(sample_id = id),
    dplyr::tbl(con, "substance") |> dplyr::select(substance_id = id)
  ) |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::compute(name = "concentration", temporary = FALSE)
  # Add extra location
  GT |> add_location(data.frame(FIPS = 20000))

  # Bad extra cols
  expect_error(
    sample_simulated_css(GT, css_extra_cols = c("no_match")),
    "No matching rows found in 'simulated_css'"
  )

  # Extra cols and substance order
  css_extra_cols <- c("sign")
  substance_order <- list(casn = c("00-00-2", "00-00-1"))
  expect_silent(
    GT <- GT |>
      sample_simulated_css(
        css_extra_cols  = css_extra_cols,
        substance_order = substance_order
      )
  )

  # Run again using stored parameters
  expect_silent(
    sample_simulated_css(GT)
  )

  # Check results
  expect_equal(GT$par$css_extra_cols, css_extra_cols)
  expect_equal(GT$par$substance_order_col, "casn")
  expect_equal(GT$par$substance_order, c("00-00-2", "00-00-1"))
  conc_tbl <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(conc_tbl$C_ss, c(1, -2, 11, -12, 21, -22, 31, -32))
})
