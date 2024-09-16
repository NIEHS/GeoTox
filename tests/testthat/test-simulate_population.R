test_that("clear downstream - age", {
  
  # Simulate GeoTox age field
  age <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  age$TOT_POP[c(1, 10)] <- 100
  geoTox <- GeoTox() |> simulate_population(age = age, n = 5)
  
  # Make C_ss fields not null
  geoTox$C_ss <- "test"
  geoTox$css_sensitivity <- "test"
  
  # Re-simulate age
  expect_warning({
    geoTox <- geoTox |> simulate_population(age = age, n = 5)
  })
  expect_null(geoTox$C_ss)
  expect_null(geoTox$css_sensitivity)
  
})

test_that("clear downstream - obesity", {
  
  # Simulate GeoTox obesity field
  obesity <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
                        OBESITY_SD = c(5, 5, 5),
                        FIPS = c("c", "a", "b"))
  geoTox <- GeoTox() |> simulate_population(obesity = obesity, n = 5)
  
  # Make C_ss fields not null
  geoTox$C_ss <- "test"
  geoTox$css_sensitivity <- "test"
  
  # Re-simulate obesity
  expect_warning({
    geoTox <- geoTox |> simulate_population(obesity = obesity, n = 5)
  })
  expect_null(geoTox$C_ss)
  expect_null(geoTox$css_sensitivity)
  
})

test_that("default params", {
  
  geoTox <- GeoTox()
  
  expect_equal(
    geoTox$par,
    list(
      n = 1e3,
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
      resp = list(tp_b_mult = 1.5)
    )
  )
  
})

test_that("update params", {
  
  # New params
  n <- 5
  IR_params <- data.frame("age" = c(20, 0, 50),
                          "mean" = c(0.3, 0.5, 0.2),
                          "sd" = 0)
  obes_prev <- "ob_p"
  obes_sd <- "ob_sd"
  obes_label <- "ob_l"
  expos_mean <- "ex_m"
  expos_sd <- "ex_sd"
  expos_label <- "ex_l"
  # time, BW, scaling and tp_b_mult are calculation fields
  
  # Create GeoTox object
  geoTox <- GeoTox()
  # Prevent IR error by setting age field
  geoTox$age <- list(c(0))
  # Update GeoTox params
  geoTox <- geoTox |> 
    simulate_population(n = n,
                        IR_params = IR_params,
                        obes_prev = obes_prev,
                        obes_sd = obes_sd,
                        obes_label = obes_label,
                        expos_mean = expos_mean,
                        expos_sd = expos_sd,
                        expos_label = expos_label)
  
  expect_equal(
    geoTox$par,
    list(
      n = n,
      IR_params = IR_params,
      obesity = list(obes_prev  = obes_prev,
                     obes_sd    = obes_sd,
                     obes_label = obes_label),
      exposure = list(expos_mean  = expos_mean,
                      expos_sd    = expos_sd,
                      expos_label = expos_label),
      internal_dose = list(time    = 1,
                           BW      = 1,
                           scaling = 1),
      resp = list(tp_b_mult = 1.5)
    )
  )
  
})
