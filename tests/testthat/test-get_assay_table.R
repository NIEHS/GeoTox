test_that("get assay table", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  expect_error(
    get_assay_table(GT),
    "Assay table not found in database."
  )
  assay_tbl <- GT |>
    add_assay(data.frame(name = "assay1", casn = "50-00-0")) |>
    get_assay_table()
  expect_equal(assay_tbl$name, "assay1")
})
