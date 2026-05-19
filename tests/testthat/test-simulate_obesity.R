test_that("missing table", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  expect_error(
    simulate_obesity(GT),
    "No 'obesity' table found in the GeoTox connection."
  )
})

test_that("duplicate location", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))
  bad_df <- data.frame(
    location_id = 1,
    OBESITY_CrudePrev = c(10, 20),
    OBESITY_SD = c(5, 10)
  )
  DBI::dbWriteTable(con, "obesity", bad_df)
  expect_error(
    simulate_obesity(GT),
    "The 'obesity' table must not contain duplicated location values."
  )
})

test_that("simulate weight", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  obesity_df <- data.frame(
    location_id = c(1, 2, 3),
    OBESITY_CrudePrev = c(10, NA_real_, 30),
    OBESITY_SD = c(5, 10, 15)
  )
  DBI::dbWriteTable(con, "obesity", obesity_df)

  # No sample table
  expect_silent(
    simulate_obesity(GT, n = 10)
  )

  # Sample table exists with age column
  expect_error(
    simulate_obesity(GT),
    "GeoTox connection already has a 'sample' table with a 'weight' column."
  )

  # Sample table exists without weight column, has an unmatched location_id
  sample_df <- data.frame(
    id = 1:6,
    location_id = c(1, 1, 2, 2, 2, 4)
  )
  DBI::dbWriteTable(con, "sample", sample_df, overwrite = TRUE)
  expect_silent(
    simulate_obesity(GT)
  )

  # Custom inputs
  colnames(obesity_df)[2:3] <- c("prev", "sd")
  DBI::dbWriteTable(con, "obesity", obesity_df, overwrite = TRUE)
  GT <- simulate_obesity(GT, obes_prev = "prev", obes_sd = "sd", overwrite = TRUE)

  # Stored GT params
  simulate_obesity(GT, overwrite = TRUE)

  # Check output values
  sample_tbl <- dplyr::tbl(con, "sample") |> dplyr::collect()
  x <- split(sample_tbl$weight, sample_tbl$location_id)
  expect_length(x[[1]], 2)
  expect_length(x[[2]], 3)
  expect_length(x[[3]], 1)
  expect_true(all(x[[1]] %in% c("Normal", "Obese")))
  expect_true(all(is.na(x[[2]])))
  expect_true(is.na(x[[3]]))
})
