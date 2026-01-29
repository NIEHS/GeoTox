test_that("missing table", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  expect_error(
    simulate_age(GT),
    "No 'age' table found in the GeoTox connection."
  )
})

test_that("incorrect nrows", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))
  bad_df <- data.frame(
    location_id = 1,
    AGEGRP = 0:9,
    TOT_POP = 100
  )
  DBI::dbWriteTable(con, "age", bad_df)
  expect_error(
    simulate_age(GT),
    "The 'age' table must contain 19 rows for each location."
  )
})

test_that("simulate age", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  age_df <- data.frame(
    location_id = rep(1:2, each = 19),
    AGEGRP = rep(0:18, times = 2),
    TOT_POP = 0
  )
  # location 1, age group 40-44
  age_df$TOT_POP[c(1, 10)]  = 100
  # location 2, age groups 50-59
  age_df$TOT_POP[c(1, 12, 13) + 19] = c(200, 100, 100)
  DBI::dbWriteTable(con, "age", age_df)

  # No sample table
  expect_silent(
    simulate_age(GT, n = 5)
  )

  # Sample table exists with age column
  expect_error(
    simulate_age(GT),
    "GeoTox connection already has a 'sample' table with an 'age' column."
  )

  # Sample table exists without age column, has an unmatched location_id (id 3)
  sample_df <- data.frame(
    id = 1:6,
    location_id = c(1, 1, 2, 2, 2, 3)
  )
  DBI::dbWriteTable(con, "sample", sample_df, overwrite = TRUE)
  # And an extra location_id in the age table (id 4)
  extra_location <- data.frame(
    location_id = 4,
    AGEGRP = 0:18,
    TOT_POP = 0
  )
  append_table(con, "age", extra_location)
  expect_silent(
    simulate_age(GT)
  )

  # Check output values
  sample_tbl <- dplyr::tbl(con, "sample") |> dplyr::collect()
  x <- split(sample_tbl$age, sample_tbl$location_id)
  expect_length(x[[1]], 2)
  expect_length(x[[2]], 3)
  expect_length(x[[3]], 1)
  expect_true(all(x[[1]] >= 40 & x[[1]] <= 44))
  expect_true(all(x[[2]] >= 50 & x[[2]] <= 59))
  expect_true(is.na(x[[3]]))
})
