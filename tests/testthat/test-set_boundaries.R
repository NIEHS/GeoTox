test_that("region", {
  
  x <- list(boundaries = NULL)
  region <- data.frame(geometry = "test")
  
  expect_error(set_boundaries(x, region = region))
  
  class(region) <- c("sf")
  
  expect_no_error(x <- set_boundaries(x, region = region))
  
  expect_equal(x$boundaries$region, region)
  expect_null(x$boundaries$group)
  
})

test_that("group", {
  
  x <- list(boundaries = NULL)
  group <- data.frame(geometry = "test")
  
  expect_error(set_boundaries(x, group = group))
  
  class(group) <- c("sf")
  
  expect_no_error(x <- set_boundaries(x, group = group))
  
  expect_null(x$boundaries$region)
  expect_equal(x$boundaries$group, group)
  
})
