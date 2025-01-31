test_that("errors", {
  # Non-numeric age
  expect_error(set_population(GeoTox(), age = letters[1:5]),
               paste0("`age` must be a numeric vector or list of ",
                      "numeric vectors"))
  # Non-character obesity
  expect_error(set_population(GeoTox(), obesity = 1:5),
               paste0("`obesity` must be a character vector or ",
                      "list of character vectors"))
  # Different sizes
  expect_error(set_population(GeoTox(), age = 1:3, obesity = "Normal"),
               "Population sizes for `age` and `obesity` do not match")
  # Obesity values
  expect_error(set_population(GeoTox(), obesity = letters[1:3]),
               "`obesity` values must be 'Normal' or 'Obese'")
})

test_that("single region", {
  age <- round(runif(10, 1, 100))
  obesity <- sample(c("Normal", "Obese"), 10, replace = TRUE)
  expect_no_error(set_population(GeoTox(), age = age))
  expect_no_error(set_population(GeoTox(), obesity = obesity))
  expect_no_error(set_population(GeoTox(), age = age, obesity = obesity))
})

test_that("multiple regions", {
  age <- list("37001" = as.integer(round(runif(10, 1, 100))),
              "37007" = round(runif(8, 1, 100)))
  obesity <- list("37001" = sample(c("Normal", "Obese"), 10, replace = TRUE),
                  "37007" = sample(c("Normal", "Obese"), 8, replace = TRUE))
  expect_no_error(set_population(GeoTox(), age = age))
  expect_no_error(set_population(GeoTox(), obesity = obesity))
  expect_no_error(set_population(GeoTox(), age = age, obesity = obesity))
})

test_that("clear downstream - age", {
  
  age <- round(runif(10, 1, 100))
  
  geoTox <- GeoTox()
  geoTox$IR <- "test"
  expect_warning(geoTox <- set_population(geoTox, age = age),
                 "Clearing `IR` field")
  expect_null(geoTox$IR)

  geoTox$C_ss <- "test"
  geoTox$css_sensitivity <- "test"
  expect_warning(geoTox <- set_population(geoTox, age = age),
                 "Clearing `C_ss` and `css_sensitivity` fields")
  expect_null(geoTox$C_ss)
  expect_null(geoTox$css_sensitivity)
})

test_that("clear downstream - obesity", {
  
  obesity <- sample(c("Normal", "Obese"), 10, replace = TRUE)
  
  geoTox <- GeoTox()
  geoTox$IR <- "test"
  expect_no_warning(geoTox <- set_population(geoTox, obesity = obesity))
  expect_false(is.null(geoTox$IR))
  
  geoTox$C_ss <- "test"
  geoTox$css_sensitivity <- "test"
  expect_warning(geoTox <- set_population(geoTox, obesity = obesity),
                 "Clearing `C_ss` and `css_sensitivity` fields")
  expect_null(geoTox$C_ss)
  expect_null(geoTox$css_sensitivity)
})
