test_that("bad inputs", {
  expect_error(sensitivity_analysis(list()))
  expect_error(sensitivity_analysis(GeoTox(), 1:3))
})

test_that("results", {
  
  # Similar to "test-compute_sensitivity.R"
  
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
  
  geoTox <- sensitivity_analysis(geoTox, list(NULL, 1.1, 1.2, 1.3, 1.4))
  
  geoTox$sensitivity

  expect_false(is.null(geoTox$sensitivity$age))
  expect_false(is.null(geoTox$sensitivity$obesity))
  expect_false(is.null(geoTox$sensitivity$css_params))
  expect_false(is.null(geoTox$sensitivity$fit_params))
  expect_false(is.null(geoTox$sensitivity$C_ext))
})
