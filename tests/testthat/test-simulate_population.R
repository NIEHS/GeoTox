test_that("simulate population", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  age_df <- data.frame(
    FIPS = rep(c("00001", "00002"), each = 19),
    AGEGRP = rep(0:18, times = 2),
    TOT_POP = 0
  )
  # location 1, age group 40-44
  age_df$TOT_POP[c(1, 10)]  = 100
  # location 2, age group 5-9 and 50-54
  age_df$TOT_POP[c(3, 12) + 19] = 100
  age_df$TOT_POP[20] <- 200

  obesity_df <- data.frame(
    FIPS = c("00001", "00002"),
    OBESITY_CrudePrev = c(10, 20),
    OBESITY_SD = c(5, 10)
  )

  exposure_df <- data.frame(
    FIPS = c("00001", "00002"),
    casn = c("50-00-0", "50-00-0"),
    route = "inhalation",
    mean = c(10, 20),
    sd = c(2, 4)
  )

  css_df <- tibble::tribble(
    ~casn, ~age_lb, ~age_ub, ~weight, ~css,
    "50-00-0",  0, 44, "Normal",  1,
    "50-00-0", 45, 89, "Normal",  2,
    "50-00-0",  0, 44, "Obese",  11,
    "50-00-0", 45, 89, "Obese",  12
  )

  expect_silent(
    GT |>
      set_simulated_css(css_df) |>
      add_exposure_rate_params() |>
      simulate_population(
        age = age_df,
        obesity = obesity_df,
        exposure = exposure_df,
        n = 3
      )
  )

  # Add another exposure substance column
  exposure_df$chnm <- c("chem1", "chem2")
  DBI::dbRemoveTable(con, "exposure")
  expect_silent(
    simulate_population(
      GT,
      exposure = exposure_df,
      substance = c("casn", "chnm"),
      simulate_rate = FALSE,
      overwrite = TRUE
    )
  )

  # Check results
  sample_df <- dplyr::tbl(con, "sample") |> dplyr::collect()
  expect_equal(nrow(sample_df), 6)
  conc_df <- dplyr::tbl(con, "concentration") |> dplyr::collect()
  expect_equal(nrow(conc_df), 6)
  expect_equal(
    colnames(conc_df),
    c("id", "sample_id", "substance_id", "route_id", "C_ext", "C_ss")
  )
})
