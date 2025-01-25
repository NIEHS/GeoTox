test_that("errors", {
  
  geoTox <- GeoTox()
  
  expect_error(plot_sensitivity(geoTox),
               "No sensitivity data found")
  
  geoTox$sensitivity <- list()
  
  expect_error(plot_sensitivity(geoTox),
               "No baseline response data found")
})

test_that("no assay", {
  
  make_data <- function(n = 5, metric = "GCA.Eff") {
    list(stats::setNames(data.frame(1:n, runif(n)),
                         c("sample", metric)))
  }
  
  x <- GeoTox()
  n <- 5
  x$resp <- make_data(n)
  x$sensitivity <- list(age = make_data(n),
                        obesity = make_data(n),
                        css_params = make_data(n),
                        fit_params = make_data(n),
                        C_ext = make_data(n))
  
  expect_no_error(plot_sensitivity(x))
})

test_that("with assay", {
  
  make_data <- function(n = 5, metric = "GCA.Eff", assay = c("a1", "a2")) {
    list(stats::setNames(data.frame(rep(assay, each = n),
                                    rep(1:n, 2),
                                    runif(2 * n)),
                         c("assay", "sample", metric)))
  }
  
  x <- GeoTox()
  n <- 5
  x$resp <- make_data(n)
  x$sensitivity <- list(age = make_data(n),
                        obesity = make_data(n),
                        css_params = make_data(n),
                        fit_params = make_data(n),
                        C_ext = make_data(n))
  
  expect_warning(plot_sensitivity(x),
                 "Multiple assays found, using first assay")
  expect_no_error(plot_sensitivity(x, assay = "a1"))
  expect_no_error(plot_sensitivity(x, assay = "a2"))
})

test_that("data NAs and missing", {
  
  make_data <- function(n = 5, metric = "GCA.Eff", p_NA = 0.5) {
    df <- stats::setNames(data.frame(1:n, runif(n)),
                          c("sample", metric))
    df[sample(n, n * p_NA), metric] <- NA
    list(df)
  }
  
  x <- GeoTox()
  n <- 5
  x$resp <- make_data(n)
  x$sensitivity <- list(age = make_data(n),
                        obesity = make_data(n),
                        css_params = make_data(n),
                        fit_params = make_data(n),
                        C_ext = make_data(n))
  
  expect_warning(plot_sensitivity(x),
                 "Removed \\d+ NA value")
  
  p_NA <- 1
  x$resp <- make_data(n, p_NA = p_NA)
  x$sensitivity <- list(age = make_data(n, p_NA = p_NA),
                        obesity = make_data(n, p_NA = p_NA),
                        css_params = make_data(n, p_NA = p_NA),
                        fit_params = make_data(n, p_NA = p_NA),
                        C_ext = make_data(n, p_NA = p_NA))
  
  expect_error(expect_warning(plot_sensitivity(x),
                              "Removed \\d+ NA value"),
               "No data to plot")
})
