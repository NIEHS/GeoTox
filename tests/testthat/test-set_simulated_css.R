test_that("set simulated css", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Missing css column
  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight,
    "00-00-1", 0, 49, "Normal",
  )
  expect_error(
    set_simulated_css(GT, css_df),
    "`df` must contain the following columns: age_lb, age_ub, weight, css."
  )

  # Okay
  css_df$css <- 1
  expect_silent(
    set_simulated_css(GT, css_df)
  )

  # Overwrite
  expect_error(
    set_simulated_css(GT, css_df),
    "Table 'simulated_css' already exists."
  )

  # Other substance column
  names(css_df)[1] <- "chem_id"
  expect_silent(
    set_simulated_css(GT, css_df, substance = "chem_id", overwrite = TRUE)
  )
})
