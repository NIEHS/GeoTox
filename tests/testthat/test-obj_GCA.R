test_that("spot check", {
  
  resp <- 1
  ln_resp <- log(resp)
  conc <- c(1, 3)
  max <- resp * 2
  AC50 <- conc
  
  expect_equal(obj_GCA(ln_resp, conc, max, AC50), 1)
  
  resp <- c(1, 2)
  ln_resp <- log(resp)
  conc <- c(1, 3)
  max <- c(3, 4)
  AC50 <- 1
  
  expect_equal(obj_GCA(ln_resp, conc, max, AC50), 16)
  
})
