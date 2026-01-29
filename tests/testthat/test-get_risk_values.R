test_that("get risk values", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup tables
  risk_df <- tibble::tribble(
    ~assay_id, ~sample_id, ~GCA.Eff,
    1, 1, 10,
    1, 2, NA_real_,
    1, 3, 30,
    1, 4, 40,
    2, 2, 60
  )
  assay_df <- tibble::tribble(
    ~id, ~name,
    1, "Assay A",
    2, "Assay B"
  )

  # Missing table
  expect_error(
    get_risk_values(GT),
    "No 'risk' table found in the GeoTox connection."
  )
  DBI::dbWriteTable(con, "risk", risk_df)

  # No assay table
  expect_warning(
    out <- get_risk_values(GT, assay = c(name = "Assay A")),
    "Ignoring assay parameter."
  )
  expect_equal(
    out,
    risk_df |> dplyr::arrange(sample_id) |> dplyr::pull(GCA.Eff)
  )

  # With assay table
  DBI::dbWriteTable(con, "assay", assay_df)
  expect_error(
    get_risk_values(GT, assay = "Assay A"),
    "A single assay must be specified with the 'assay' input as a named vector"
  )
  expect_error(
    get_risk_values(GT, assay = c(bad_name = "Assay A")),
    "Column 'bad_name' not found in assay table."
  )
  expect_error(
    get_risk_values(GT, assay = c(name = "Missing Assay")),
    "No results found for assay 'Missing Assay'."
  )
  out <- get_risk_values(GT, assay = c(name = "Assay A"))
  expect_equal(out, c(10, NA_real_, 30, 40))
  out <- get_risk_values(GT, assay = c(name = "Assay B"))
  expect_equal(out, 60)
  out <- get_risk_values(GT, assay = c(id = 2))
  expect_equal(out, 60)
})
