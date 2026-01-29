test_that("table helpers", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Check par table
  invisible(set_par(con, "other_params", c("a", "b", "c")))
  expect_equal(
    get_par(con),
    list(
      # ordered by name, idx
      other_params = c("a", "b", "c"),
      reset_seed = FALSE
    )
  )

  # Start with a location table, use reset_seed
  location_df <- data.frame(FIPS = c("00001", "00002"))
  seed <- .Random.seed
  add_table(con, "location", location_df, reset_seed = TRUE)
  expect_equal(
    dplyr::tbl(con, "location") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~FIPS,
      1, "00001",
      2, "00002"
    )
  )
  expect_identical(.Random.seed, seed)

  # Add new rows/columns to location table, use reset_seed
  location_df <- data.frame(
    FIPS = c("00001", "00003"),
    area = c(10, 20),
    zip = c(12345L, 54321L)
  )
  seed <- .Random.seed
  add_table(con, "location", location_df, reset_seed = TRUE)
  expect_equal(
    dplyr::tbl(con, "location") |> dplyr::collect(),
    tibble::tribble(
      ~id,   ~FIPS, ~area,   ~zip,
        1, "00001",    10, 12345L,
        2, "00002",    NA,     NA,
        3, "00003",    20, 54321L
    )
  )
  expect_identical(.Random.seed, seed)

  # Reserved column
  test_df <- data.frame(id = 1, key_id = 1)
  foreign_keys <- list(key_id = "key_id")
  expect_error(
    add_table(con, "test", test_df, foreign_keys = foreign_keys),
    "`df` cannot contain column\\(s\\) 'id', 'key_id'."
  )

  # Bad foreign keys
  test_df <- data.frame(loc = c("00001"), new_col = 50)
  foreign_keys <- list(location_id = c())
  expect_error(
    add_table(con, "test", test_df, foreign_keys),
    "`location` must be a character vector."
  )
  foreign_keys <- list(location_id = c(FIPS = "loc", "other"))
  expect_error(
    add_table(con, "test", test_df, foreign_keys),
    "Each `location` value must be a column name in `df`."
  )
  foreign_keys <- list(location_id = c(FIPS = "loc", "loc"))
  expect_error(
    add_table(con, "test", test_df, foreign_keys),
    "If `location` is a named character vector, then all values must be named."
  )

  # Add sample table with named foreign keys, don't use reset_seed
  sample_df <- data.frame(loc = c("00001"), age = 50)
  foreign_keys <- list(location_id = c(FIPS = "loc"))
  seed <- .Random.seed
  add_table(con, "sample", sample_df, foreign_keys)
  expect_equal(
    dplyr::tbl(con, "sample") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~location_id, ~age,
      1, 1, 50
    )
  )
  expect_false(identical(.Random.seed, seed))

  # Add to empty table with unnamed foreign key
  empty_df <- data.frame(id = integer(0), location_id = integer(0))
  DBI::dbWriteTable(con, "empty", empty_df)
  empty_df <- data.frame(FIPS = c("00002"), new_col = "text")
  foreign_keys <- list(location_id = "FIPS")
  add_table(con, "empty", empty_df, foreign_keys)
  expect_equal(
    dplyr::tbl(con, "empty") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~location_id, ~new_col,
      1, 2, "text"
    )
  )

  # Add to table with no overlapping columns
  location_df <- data.frame(ST = c("XYZ"))
  add_table(con, "location", location_df)
  expect_equal(
    dplyr::tbl(con, "location") |> dplyr::collect(),
    tibble::tribble(
      ~id,   ~FIPS, ~area,   ~zip,   ~ST,
        1, "00001",    10, 12345L,    NA,
        2, "00002",    NA,     NA,    NA,
        3, "00003",    20, 54321L,    NA,
        4,      NA,    NA,     NA, "XYZ"
    )
  )

})
