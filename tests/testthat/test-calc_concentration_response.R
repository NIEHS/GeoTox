test_that("C_invitro with basic hill_params", {
  
  col_names <- c("sample", "GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10")
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1), resp = c(10, 5, 0)))
  
  C_invitro <- matrix(0)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(col_names %in% names(out[[1]])))
  expect_true(all(unname(out[[1]]) == c(1, NA, NA, NA, NA)))
  
  C_invitro <- matrix(1)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(col_names %in% names(out[[1]])))
  
  C_invitro <- matrix(1:4, ncol = 1)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(col_names %in% names(out[[1]])))
  
  C_invitro <- list(matrix(1))
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(col_names %in% names(out[[1]])))
  
  C_invitro <- list(matrix(1:4), matrix(5:8))
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_length(out, 2)
  expect_true(all(col_names %in% names(out |> do.call(what = rbind))))
  
  C_invitro <- matrix(1:4, ncol = 2)
  # hill_params needs "chem" column if C_invitro has > 1 row
  expect_error(calc_concentration_response(C_invitro, hill_params))
})

test_that("hill_params 'chem'", {
  
  col_names <- c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10")
  
  # Single chemical
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1),
                                     resp = c(10, 5, 0),
                                     chem = rep("c1", each = 3)),
                          chem = "chem")
  
  C_invitro <- matrix(1:4, ncol = 1)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(c(col_names, "sample") %in% names(out[[1]])))
  expect_true(nrow(out[[1]]) == 4)
  
  # Multiple chemicals
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1, -2, -1, 0),
                                     resp = c(10, 5, 0, 0, 1, 2),
                                     chem = rep(c("c1", "c2"), each = 3)),
                          chem = "chem")
  
  C_invitro <- matrix(1:4, ncol = 2)
  expect_error(calc_concentration_response(C_invitro, hill_params))
  
  colnames(C_invitro) <- c("c1", "c2")
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(c(col_names, "sample") %in% names(out[[1]])))
  expect_true(nrow(out[[1]]) == 2)
})

test_that("hill_params 'assay'", {
  
  col_names <- c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10")
  
  # Single assay
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1),
                                     resp = c(10, 5, 0),
                                     assay = rep("a1", each = 3)),
                          assay = "assay")
  
  C_invitro <- matrix(1:4, ncol = 1)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(c(col_names, "assay") %in% names(out[[1]])))
  expect_true(nrow(out[[1]]) == 4)

  # Multiple assays
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1, -2, -1, 0),
                                     resp = c(10, 5, 0, 0, 1, 2),
                                     assay = rep(c("a1", "a2"), each = 3)),
                          assay = "assay")
  
  C_invitro <- matrix(1:4, ncol = 1)
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(c(col_names, "assay") %in% names(out[[1]])))
  expect_true(nrow(out[[1]]) == 8)
})

test_that("hill_params 'assay' and 'chem'", {
  
  col_names <- c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10")
  
  df <- data.frame(logc = rep(c(-1, 0, 1, -2, -1, 0), times = 2),
                   resp = rep(c(10, 5, 0, 0, 1, 2), times = 2),
                   chem = rep(rep(c("c1", "c2"), each = 3), times = 2),
                   assay = rep(rep(c("a1", "a2"), each = 6)))
  hill_params <- fit_hill(df, chem = "chem", assay = "assay")
  
  C_invitro <- matrix(1:4, ncol = 2, dimnames = list(NULL, c("c1", "c2")))
  
  expect_no_error(out <- calc_concentration_response(C_invitro, hill_params))
  expect_true(all(c(col_names, "assay", "sample") %in% names(out[[1]])))
  expect_true(nrow(out[[1]]) == 4)
})

test_that("other inputs", {
  
  hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1), resp = c(10, 5, 0)))
  C_invitro <- matrix(1)
  
  expect_no_error(calc_concentration_response(C_invitro,
                                              hill_params,
                                              max_mult = 1.1))
  expect_no_error(calc_concentration_response(C_invitro,
                                              hill_params,
                                              fixed = TRUE))
})
