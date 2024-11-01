test_that("warnings and errors", {
  
  df <- data.frame(id = c("r1", "r2"),
                   metric = "GCA.Eff",
                   assay_quantile = 0.5,
                   value = 1:2)
  
  unit_square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  
  region_boundary <- data.frame(
    region = c("r1", "r2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square)),
      sf::st_polygon(list(unit_square + 1))
    )
  )
  
  expect_no_error(plot_resp(df, region_boundary))
  
  df$value <- NA
  
  expect_no_error(plot_resp(df, region_boundary))
  
  expect_error(plot_resp(df, region_boundary, assay_quantiles = 0.5),
               "Both assay_quantiles and summary_quantiles must be named")
  expect_error(plot_resp(df, region_boundary, summary_quantiles = 0.1),
               "Both assay_quantiles and summary_quantiles must be named")
  
  region_boundary <- region_boundary[1, , drop = FALSE]
  
  expect_warning(plot_resp(df, region_boundary),
                 "Some response data was removed due to missing spatial data")
  
  region_boundary$region <- "r3"
  
  expect_error(plot_resp(df, region_boundary),
               "No spatial data for corresponding response data")
})

test_that("no summary data", {
  
  assay_quantiles <- c("aq25" = 0.25, "aq75" = 0.75)
  
  df <- data.frame(id = rep(c("r1", "r2"), each = 2),
                   metric = "GCA.Eff",
                   assay_quantile = rep(assay_quantiles, 2),
                   value = 1:4)
  
  unit_square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  
  region_boundary <- data.frame(
    region = c("r1", "r2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square)),
      sf::st_polygon(list(unit_square + 1))
    )
  )
  
  group_boundary <- data.frame(
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square * 2))
    )
  )
  
  expect_no_error(plot_resp(df, region_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary,
                            assay_quantiles))
})

test_that("with summary data", {
  
  assay_quantiles <- c("aq25" = 0.25, "aq75" = 0.75)
  summary_quantiles = c("sq10" = 0.1, "sq90" = 0.9)
  
  df <- data.frame(id = rep(c("r1", "r2"), each = 4),
                   assay_quantile = rep(rep(assay_quantiles, 2), each = 2),
                   metric = "GCA.Eff",
                   summary_quantile = rep(summary_quantiles, 4),
                   value = 1:8)
  
  unit_square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  
  region_boundary <- data.frame(
    region = c("r1", "r2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square)),
      sf::st_polygon(list(unit_square + 1))
    )
  )
  
  group_boundary <- data.frame(
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square * 2))
    )
  )
  
  expect_no_error(plot_resp(df, region_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary,
                            assay_quantiles))
  expect_no_error(plot_resp(df, region_boundary, group_boundary,
                            assay_quantiles, summary_quantiles))
})

test_that("with assay", {
  
  assay_quantiles <- c("aq25" = 0.25, "aq75" = 0.75)
  
  df <- data.frame(id = rep(c("r1", "r2"), each = 2),
                   assay = rep("a1", 4),
                   metric = "GCA.Eff",
                   assay_quantile = rep(assay_quantiles, 2),
                   value = 1:4)
  
  unit_square <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  
  region_boundary <- data.frame(
    region = c("r1", "r2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square)),
      sf::st_polygon(list(unit_square + 1))
    )
  )
  
  group_boundary <- data.frame(
    geometry = sf::st_sfc(
      sf::st_polygon(list(unit_square * 2))
    )
  )
  
  expect_no_error(plot_resp(df, region_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary))
  expect_no_error(plot_resp(df, region_boundary, group_boundary,
                            assay_quantiles))
})
