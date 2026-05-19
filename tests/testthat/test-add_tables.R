test_that("add tables", {
  GT <- GeoTox(withr::local_tempfile(fileext = ".duckdb"))
  con <- get_con(GT)
  withr::defer(DBI::dbDisconnect(con))

  #=====================================
  # Tables w/o foreign keys
  #=====================================

  # exposure_rate_params
  params_df <- tibble::tribble(
    ~age_lb, ~age_ub, ~gender, ~other, ~mean, ~sd,
     0, 49, "male",   "other", 10, 2,
    50, 99, "male",   "other", 20, 3,
     0, 49, "female", "other", 15, 4,
    50, 99, "female", "other", 25, 5
  )
  GT |>
    add_exposure_rate_params() |>
    add_exposure_rate_params(route = "drinking") |>
    add_exposure_rate_params(route = "custom", params = params_df)
  # no default values
  expect_error(
    add_exposure_rate_params(GT, route = "dermal"),
    "Default exposure params do not exist for route 'dermal'."
  )
  # bad colnames
  params_df <- tibble::tribble(
    ~age_lb, ~age_ub, ~gender, ~other, ~mean,
     0, 49, "male",   "other", 10,
    50, 99, "male",   "other", 20,
     0, 49, "female", "other", 15,
    50, 99, "female", "other", 25
  )
  expect_error(
    add_exposure_rate_params(GT, route = "bad_params", params = params_df),
    "`params` must contain the following columns:"
  )
  params_df$sd <- 2
  params_df$route <- "bad_params"
  expect_error(
    add_exposure_rate_params(GT, route = "bad_params", params = params_df),
    "`params` should not contain a 'route' column."
  )
  # overwrite
  expect_error(
    add_exposure_rate_params(GT),
    "Exposure rate parameters for route 'inhalation' already exist."
  )
  add_exposure_rate_params(GT, overwrite = TRUE)
  expect_equal(
    dplyr::tbl(con, "exposure_rate_params") |> dplyr::collect() |>
      dplyr::summarize(n = dplyr::n(), .by = route_id) |>
      dplyr::arrange(route_id) |> dplyr::collect(),
    tibble::tribble(
      ~route_id, ~n,
      1, 10,
      2, 10,
      3, 4
    )
  )

  # location
  location_df <- data.frame(FIPS = c("00001", "00002"))
  add_location(GT, location_df)
  expect_equal(
    dplyr::tbl(con, "location") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~FIPS,
      1, "00001",
      2, "00002"
    )
  )

  # route
  route_df <- data.frame(route = "dermal")
  add_route(GT, route_df)
  expect_equal(
    dplyr::tbl(con, "route") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~route,
      1, "inhalation",
      2, "drinking",
      3, "custom",
      4, "dermal"
    )
  )

  # substance
  substance_df <- data.frame(
    casn = c("50-00-0", "50-00-1"),
    chnm = c("chem1", "chem2")
  )
  add_substance(GT, substance_df)
  expect_equal(
    dplyr::tbl(con, "substance") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~casn, ~chnm,
      1, "50-00-0", "chem1",
      2, "50-00-1", "chem2"
    )
  )

  #=====================================
  # Tables w/ foreign keys
  #=====================================

  # age
  age_df <- tibble::tribble(
    ~FIPS, ~age_group, ~count,
    "00001", 0, 1000,
    "00001", 1, 1200,
    "00002", 0, 800,
    "00002", 1, 900
  )
  add_age(GT, age_df)
  expect_equal(
    dplyr::tbl(con, "age") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~location_id, ~age_group, ~count,
      1, 1, 0, 1000,
      2, 1, 1, 1200,
      3, 2, 0, 800,
      4, 2, 1, 900
    )
  )

  # assay
  assay_df <- tibble::tribble(
    ~name, ~casn,
    "assay1", "50-00-0",
    "assay2", "50-00-0",
    "assay1", "50-00-1"
  )
  add_assay(GT, assay_df)
  expect_equal(
    dplyr::tbl(con, "assay") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~substance_id, ~name,
      1, 1, "assay1",
      2, 1, "assay2",
      3, 2, "assay1"
    )
  )

  # exposure
  exposure_df <- tibble::tribble(
    ~FIPS, ~casn, ~route, ~value,
    "00001", "50-00-0", "inhalation", 5,
    "00001", "50-00-1", "drinking", 10,
    "00002", "50-00-0", "inhalation", 7
  )
  add_exposure(GT, exposure_df)
  expect_equal(
    dplyr::tbl(con, "exposure") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~location_id, ~substance_id, ~route_id, ~value,
      1, 1, 1, 1, 5,
      2, 1, 2, 2, 10,
      3, 2, 1, 1, 7
    )
  )

  # obesity
  obesity_df <- tibble::tribble(
    ~FIPS, ~prev,
    "00001", 0.3,
    "00002", 0.25
  )
  add_obesity(GT, obesity_df)
  expect_equal(
    dplyr::tbl(con, "obesity") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~location_id, ~prev,
      1, 1, 0.3,
      2, 2, 0.25
    )
  )

  #=====================================
  # Tables w/ optional foreign keys
  #=====================================

  # hill_params w/o foreign keys
  hill_params <- list(
    fit = data.frame(par1 = 1:2, par2 = 3:4),
    assay = NULL,
    substance = NULL
  )
  add_hill_params(GT, hill_params)
  expect_equal(
    dplyr::tbl(con, "hill_params") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~par1, ~par2,
      1, 1, 3,
      2, 2, 4
    )
  )

  # hill_params w/o foreign keys
  hill_params$fit$name = c("assay1", "assay2")
  hill_params$fit$casn = c("50-00-0", "50-00-1")
  hill_params$assay = "name"
  hill_params$substance = "casn"
  add_hill_params(GT, hill_params)
  expect_equal(
    dplyr::tbl(con, "hill_params") |> dplyr::collect(),
    tibble::tribble(
      ~id, ~par1, ~par2, ~assay_id, ~substance_id,
      1, 1, 3, 1, 1,
      2, 2, 4, 2, 2
    )
  )
})
