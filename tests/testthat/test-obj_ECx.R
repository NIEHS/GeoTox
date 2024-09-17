test_that("spot check", {
  
  conc_mix <- 2
  resp <- 1
  conc <- c(1, 3)
  max <- resp * 2
  AC50 <- conc
  
  expect_equal(obj_ECx(conc_mix, resp, conc, max, AC50), 0)
  
  conc_mix <- 2
  resp <- c(1, 2)
  conc <- c(1, 3)
  max <- c(3, 4)
  AC50 <- 1
  
  expect_equal(obj_ECx(conc_mix, resp, conc, max, AC50), 2.25)
  
})
