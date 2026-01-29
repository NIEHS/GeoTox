test_that("missing tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  expect_error(
    set_fixed_css(GT),
    "No 'sample' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "sample", data.frame(id = 1, location_id = 1))

  expect_error(
    set_fixed_css(GT),
    "No 'simulated_css' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "simulated_css", data.frame(id = 1, location_id = 1))

  expect_error(
    set_fixed_css(GT),
    "No 'substance' table found in the GeoTox connection."
  )

  DBI::dbWriteTable(con, "substance", data.frame(id = 1, location_id = 1))

  expect_error(
    set_fixed_css(GT),
    "No 'concentration' table found in the GeoTox connection."
  )
})

test_that("set fixed css", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  set.seed(1234)

  # Setup tables
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight,
    10000, 30, "Normal",
    10000, 50, "Normal",
    10000, 30,  "Obese",
    10000, 50,  "Obese",
    20000, 60, "Normal",
    20000, 40, "Normal",
    20000, 60,  "Obese",
    20000, 40,  "Obese"
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
  )
  GT |> set_sample(sample_df) |> set_simulated_css(css_df)
  conc_df <- dplyr::cross_join(
    dplyr::tbl(con, "sample") |> dplyr::select(sample_id = id),
    dplyr::tbl(con, "substance") |> dplyr::select(substance_id = id)
  ) |>
    dplyr::arrange(substance_id, sample_id) |>
    dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
    dplyr::compute(name = "concentration", temporary = FALSE)
  sample_simulated_css(GT)

  # Basic use
  set_fixed_css(GT)

  # Input substance_order
  GT <- GT |>
    set_fixed_css(substance_order = list(casn = c("00-00-2", "00-00-1")))
  expect_equal(GT$par$substance_order_col, "casn")
  expect_equal(GT$par$substance_order, c("00-00-2", "00-00-1"))

  # Using stored GT$par
  set_fixed_css(GT)

  # Check results
  fixed_css_tbl <- dplyr::tbl(con, "fixed_css") |>
    dplyr::collect() |> dplyr::arrange(id)
  expect_equal(
    fixed_css_tbl,
    tibble::tribble(
      ~id, ~sample_id, ~substance_id, ~age, ~weight, ~params, ~other,
       1, 1, 1,  1,    1,  1, 16.5,
       2, 2, 1,  7,    1,  1, 16.5,
       3, 3, 1,  1, 11.5,  1, 16.5,
       4, 4, 1,  7, 11.5,  1, 16.5,
       5, 5, 1,  7,    1,  2, 16.5,
       6, 6, 1,  1,    1,  2, 16.5,
       7, 7, 1,  7, 11.5,  2, 16.5,
       8, 8, 1,  1, 11.5,  2, 16.5,
       9, 1, 2, 26, 21.5, 21, 16.5,
      10, 2, 2, 27, 21.5, 21, 16.5,
      11, 3, 2, 26, 31.5, 21, 16.5,
      12, 4, 2, 27, 31.5, 21, 16.5,
      13, 5, 2, 27, 21.5, 22, 16.5,
      14, 6, 2, 26, 21.5, 22, 16.5,
      15, 7, 2, 27, 31.5, 22, 16.5,
      16, 8, 2, 26, 31.5, 22, 16.5
    )
  )
})
