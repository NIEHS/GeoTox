test_that("errors", {
  # If IR_params is input, then age must be input or already exist
  expect_error(GeoTox() |> simulate_population(IR_params = "test"))
})

test_that("populate fields - age", {

  geoTox <- GeoTox()
  age <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
  age$TOT_POP[c(1, 10)] <- 100
  
  expect_null(geoTox$age)
  expect_null(geoTox$IR)
  
  geoTox <- geoTox |> simulate_population(age = age, n = 5)
  
  expect_false(is.null(geoTox$age))
  expect_false(is.null(geoTox$IR))
})

test_that("populate fields - IR_params", {
  
  geoTox <- GeoTox()
  IR_params <- data.frame("age" = c(20, 0, 50),
                          "mean" = c(0.3, 0.5, 0.2),
                          "sd" = 0)
  
  expect_null(geoTox$IR)
  
  geoTox$age <- list(seq(10, 90, by = 10))
  geoTox <- geoTox |> simulate_population(IR_params = IR_params)
  
  expect_false(is.null(geoTox$IR))
})

test_that("populate fields - obesity", {
  
  geoTox <- GeoTox()
  obesity <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
                        OBESITY_SD = c(5, 5, 5),
                        FIPS = c("c", "a", "b"))
  
  expect_null(geoTox$obesity)
  
  geoTox <- geoTox |> simulate_population(obesity = obesity, n = 5)

  expect_false(is.null(geoTox$obesity))
})

test_that("populate fields - exposure", {
  
  geoTox <- GeoTox()
  exposure <- data.frame(mean = 10, sd = 1)
  
  expect_null(geoTox$exposure)
  expect_null(geoTox$C_ext)
  
  geoTox <- geoTox |> simulate_population(exposure = exposure, n = 5)
  
  expect_false(is.null(geoTox$exposure))
  expect_false(is.null(geoTox$C_ext))
})

test_that("populate fields - simulated_css", {
  
  geoTox <- GeoTox()
  x <- y <- expand.grid(age_min = seq(0, 50, 10), weight = c("Normal", "Obese"))
  x$css <- lapply(1:nrow(x), function(i) {
    rep(x$age_min[i] / 5 + as.integer(x$weight[i] == "Obese"), 2)
  })
  x$age_median_css <- x$weight_median_css <- 10
  y$css <- lapply(1:nrow(y), function(i) {
    rep(y$age_min[i] / 10 + as.integer(y$weight[i] == "Obese") / 2, 2)
  })
  y$age_median_css <- y$weight_median_css <- 10
  simulated_css <- list("chem1" = x, "chem2" = y)

  expect_null(geoTox$C_ss)
  expect_null(geoTox$css_sensitivity)
  
  geoTox$age <- seq(5, 50, by = 5)
  geoTox$obesity <- rep("Normal", 10)
  geoTox <- geoTox |> simulate_population(simulated_css = simulated_css)
  
  expect_false(is.null(geoTox$C_ss))
  expect_false(is.null(geoTox$css_sensitivity))
})

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
                    resp = list(tp_b_mult = 1.5)))
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
  
  expect_equal(geoTox$par,
               list(n = n,
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
                    resp = list(tp_b_mult = 1.5)))
})
