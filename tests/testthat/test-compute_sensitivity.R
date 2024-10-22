test_that("bad inputs", {
  
  # Age field not set
  expect_error(compute_sensitivity(GeoTox()))
  
  # IR field not set
  geoTox <- GeoTox()
  geoTox$age <- list(1)
  expect_error(compute_sensitivity(geoTox))
  
  # C_ext field not set
  geoTox <- GeoTox()
  geoTox$age <- list(1)
  expect_error(compute_sensitivity(geoTox, vary = "C_ext"))
})

test_that("results", {
  
  # Similar to "test-sensitivity_analysis.R"
  
  geoTox <- GeoTox()
  
  expect_null(geoTox$sensitivity)
  
  geoTox$par$n <- 1
  geoTox$hill_params <- data.frame(resp_max = 5,
                                   tp = 5,
                                   tp.sd = 1,
                                   logc_min = -1,
                                   logc_max = 1,
                                   logAC50 = 0,
                                   logAC50.sd = 1)
  geoTox$age <- list(2)
  geoTox$IR <- list(0.5)
  geoTox$obesity <- list("Normal")
  geoTox$exposure <- list(data.frame(mean = 1, sd = 0))
  geoTox$C_ext <- list(1)
  geoTox$css_sensitivity <- list(age = list(matrix(5)),
                                 obesity = list(matrix(10)),
                                 params = list(matrix(15)),
                                 other = list(7))
  
  out <- compute_sensitivity(geoTox, "age", NULL)
  
  expect_type(out, "list")
  expect_length(out, 1)
  
  out <- compute_sensitivity(geoTox, "obesity", 1.1)
  
  expect_type(out, "list")
  expect_length(out, 1)
  
  out <- compute_sensitivity(geoTox, "css_params", 1.2)
  
  expect_type(out, "list")
  expect_length(out, 1)
  
  out <- compute_sensitivity(geoTox, "fit_params", 1.3)
  
  expect_type(out, "list")
  expect_length(out, 1)
  
  out <- compute_sensitivity(geoTox, "C_ext", 1.4)
  
  expect_type(out, "list")
  expect_length(out, 1)
})
