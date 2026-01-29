test_that("set sample", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Normal inputs
  sample_df <- tibble::tribble(
    ~FIPS, ~age, ~weight,
    10000, 25, "Normal"
  )
  set_sample(GT, sample_df)
  sample_tbl <- DBI::dbReadTable(con, "sample")
  expect_equal(sample_tbl$age, 25)
  location_tbl <- DBI::dbReadTable(con, "location")
  expect_equal(nrow(location_tbl), 1)
  expect_equal(colnames(location_tbl), c("id", "FIPS"))

  # Concentration and risk tables exit
  DBI::dbWriteTable(con, "concentration", tibble::tibble(id = 1))
  DBI::dbWriteTable(con, "risk", tibble::tibble(id = 1))
  expect_error(
    set_sample(GT, sample_df),
    "The 'concentration' or 'risk' tables already exist."
  )
  expect_silent(
    set_sample(GT, sample_df, overwrite = TRUE)
  )
  expect_true(!DBI::dbExistsTable(con, "concentration"))
  expect_true(!DBI::dbExistsTable(con, "risk"))

  # Different location
  sample_df <- tibble::tribble(
    ~FIPS, ~zip, ~age,
    20000, "12345", 30
  )
  # 'concentration' and 'risk' tables don't exist, so overwrite isn't needed
  set_sample(GT, sample_df, location = c("FIPS", "zip"))
  sample_tbl <- DBI::dbReadTable(con, "sample")
  expect_equal(sample_tbl$age, 30)
  location_tbl <- DBI::dbReadTable(con, "location")
  expect_equal(nrow(location_tbl), 2)
  expect_equal(colnames(location_tbl), c("id", "FIPS", "zip"))
  expect_true(is.na(location_tbl$zip[[1]]))
})
