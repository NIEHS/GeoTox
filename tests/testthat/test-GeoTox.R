test_that("errors", {
  
  geoTox <- GeoTox()
  
  expect_error(plot(geoTox),
               "No response data found")
  
  geoTox$resp <- list()
  
  expect_error(plot(geoTox),
               "No region boundary data found")
})

test_that("default params", {
  
  geoTox <- GeoTox()
  
  expect_equal(geoTox$par,
               list(n = 1e3,
                    IR_params = NULL,
                    obesity = list(obes_prev  = "OBESITY_CrudePrev",
                                   obes_sd    = "OBESITY_SD",
                                   obes_label = "FIPS"),
                    exposure = list(expos_mean  = "mean",
                                    expos_sd    = "sd",
                                    expos_label = "casn"),
                    internal_dose = list(time    = 1,
                                         BW      = 1,
                                         scaling = 1),
                    resp = list(max_mult = 1.5)))
})

test_that("package data subset", {
  
  set.seed(2357)
  n <- 10 # Population size
  m <- 5 # Number of regions
  idx <- if (m < 100) sample(1:100, m) else 1:100

  expect_no_error(
    geoTox <- GeoTox() |>
      # Set region and group boundaries (for plotting)
      set_boundaries(region = geo_tox_data$boundaries$county,
                     group  = geo_tox_data$boundaries$state) |>
      # Simulate populations for each region
      simulate_population(age           = split(geo_tox_data$age, ~FIPS)[idx],
                          obesity       = geo_tox_data$obesity[idx, ],
                          exposure      = split(geo_tox_data$exposure, ~FIPS)[idx],
                          simulated_css = geo_tox_data$simulated_css,
                          n             = n) |> 
      # Estimated Hill parameters
      set_hill_params(geo_tox_data$dose_response |>
                        fit_hill(assay = "endp", chem = "casn") |>
                        dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
      # Calculate response
      calculate_response() |>
      # Perform sensitivity analysis
      sensitivity_analysis()
  )
  
  # Print GeoTox object
  expect_no_error(capture_output(print(geoTox)))
  
  # Plot hill fits
  expect_no_error(plot(geoTox, type = "hill"))
  # Plot exposure data
  expect_no_error(plot(geoTox, type = "exposure"))
  # Plot response data
  # Warning from resp_quantiles()
  expect_warning(plot(geoTox),
                 "Multiple assays found, using first assay")
  expect_no_error(plot(geoTox, assays = "TOX21_H2AX_HTRF_CHO_Agonist_ratio"))
  # Plot sensitivity data
  expect_warning(expect_warning(
    plot(geoTox, type = "sensitivity"),
    "Multiple assays found, using first assay"),
    "Removed \\d+ NA value")
  expect_no_error(plot(geoTox,
                       type = "sensitivity",
                       assay = "TOX21_H2AX_HTRF_CHO_Agonist_ratio"))
})

test_that("print corner cases", {
  
  geoTox <- GeoTox()
  
  expect_no_error(capture_output(print(geoTox)))
  
  geoTox$age <- list()
  geoTox$obesity <- c(0)
  geoTox$par <- NULL
  
  expect_no_error(capture_output(print(geoTox)))
  
  geoTox <- GeoTox() |>
    set_hill_params(geo_tox_data$dose_response |>
                      dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
                                    casn == "510-15-6") |>
                      fit_hill())

  expect_no_error(capture_output(print(geoTox)))
  
  geoTox <- geoTox |>
    set_hill_params(geo_tox_data$dose_response |>
                      fit_hill(assay = "endp", chem = "casn"))
  
  expect_no_error(capture_output(print(geoTox)))
  
  geoTox$resp <- c(1:5)
  
  expect_no_error(capture_output(print(geoTox)))
})
