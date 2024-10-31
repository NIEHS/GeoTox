test_that("missing input", {
  expect_error(plot_exposure(NULL),
               "No exposure data found")
  expect_error(plot_exposure(c(1), NULL),
               "No region_boundary data found")
})

test_that("generates plots", {
  
  exposure <- split(data.frame(region = rep(c("r1", "r2"), each = 2),
                               chnm = rep(c("c1", "c2"), 2),
                               mean = c(0, 1, 2, 3),
                               norm = c(1, 2, 3, 4)),
                    ~region)
  
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
  
  expect_no_error(plot_exposure(exposure, region_boundary))
  expect_no_error(plot_exposure(exposure, region_boundary, group_boundary))
})
