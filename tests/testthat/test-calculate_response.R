test_that("expected inputs", {
  
  geoTox <- GeoTox()
  
  expect_error(calculate_response(geoTox),
               "GeoTox object must contain 'IR' and 'C_ext' fields")
  
  geoTox$IR <- 2
  geoTox$C_ext <- matrix(3)
  
  expect_error(calculate_response(geoTox),
               "GeoTox object must contain 'C_ss' field")
  
  geoTox$C_ss <- 5
  
  expect_error(calculate_response(geoTox),
               "GeoTox object must contain 'hill_params' field")
  
  geoTox$hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1),
                                            resp = c(10, 5, 0)))
  
  expect_no_error(calculate_response(geoTox))
})

test_that("update params", {
  
  # New params
  time <- 2
  BW <- 3
  scaling <- 4
  max_mult <- 5

  # Create GeoTox object
  geoTox <- GeoTox()
  
  # Prevent errors from missing data
  geoTox$IR <- 2
  geoTox$C_ext <- matrix(3)
  geoTox$C_ss <- 5
  geoTox$hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1),
                                            resp = c(10, 5, 0)))
  
  # Update GeoTox params
  geoTox <- geoTox |> 
    calculate_response(time = time,
                       BW = BW,
                       scaling = scaling,
                       max_mult = max_mult)

  expect_equal(geoTox$par,
               list(n = 1e3,
                    IR_params = NULL,
                    obesity = list(obes_prev  = "OBESITY_CrudePrev",
                                   obes_sd    = "OBESITY_SD",
                                   obes_label = "FIPS"),
                    exposure = list(expos_mean  = "mean",
                                    expos_sd    = "sd",
                                    expos_label = "casn"),
                    internal_dose = list(time    = time,
                                         BW      = BW,
                                         scaling = scaling),
                    resp = list(max_mult = max_mult)))
})
