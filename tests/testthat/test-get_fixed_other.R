test_that("expected results", {
  C_ss <- list(matrix(1:6,
                      ncol = 2,
                      dimnames = list(NULL, c("chem1", "chem2"))),
               matrix(7:12,
                      ncol = 2,
                      dimnames = list(NULL, c("chem1", "chem2"))))
  
  expect_equal(
    get_fixed_other(C_ss),
    list(rep(median(1:6), 3), rep(median(7:12), 3))
  )
  
  # Add missing values
  C_ss[[1]][3, 1] <- C_ss[[1]][2, 2] <- NA
  C_ss[[2]][1, 2] <- C_ss[[2]][1, 1] <- NA
  
  expect_equal(
    get_fixed_other(C_ss),
    list(rep(median(c(1, 2, mean(c(1, 2)), 4, mean(c(4, 6)), 6)), 3),
         rep(median(c(mean(c(8, 9)), 8, 9, mean(c(11, 12)), 11, 12)), 3))
  )
})
