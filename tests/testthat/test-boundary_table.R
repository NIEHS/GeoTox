test_that("boundary table", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  # Setup sf objects
  county <- sf::st_sf(
    FIPS = c("00001", "00002"),
    geometry = sf::st_sfc(sf::st_point(1:2), sf::st_point(3:4))
  )
  state <- sf::st_sf(
    STATE = "XYZ",
    geometry = sf::st_sfc(sf::st_point(5:6))
  )
  df_list <- list(county = county, state = state)

  # Bad df_list
  expect_error(
    set_boundary(GT, list(county = data.frame(a = 1))),
    "`df_list` must be a named list of sf objects."
  )

  # No 'boundary' table
  expect_error(
    get_boundary(GT),
    "No 'boundary' table found in the GeoTox connection."
  )

  # Set boundary
  set_boundary(GT, df_list)

  # Overwrite
  expect_error(
    set_boundary(GT, df_list),
    "GeoTox connection already has a 'boundary' table."
  )
  set_boundary(GT, df_list, overwrite = TRUE)

  # Get boundary
  res <- get_boundary(GT) |> tibble::deframe()
  expect_equal(res$county$geometry, county$geometry)
  expect_equal(res$county$location_id, 1:nrow(county))
  expect_equal(res$state$geometry, state$geometry)
  expect_equal(res$state$STATE, state$STATE)
})
