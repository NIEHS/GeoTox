test_that("metrics", {
  
  resp <- list(
    "r1" = data.frame(sample = 1,
                      GCA.Eff = 2,
                      IA.Eff = 3,
                      GCA.HQ.10 = 4,
                      IA.HQ.10 = 5))
  
  expect_no_error(resp_quantiles(resp, metric = "GCA.Eff"))
  expect_no_error(resp_quantiles(resp, metric = "IA.Eff"))
  expect_no_error(resp_quantiles(resp, metric = "GCA.HQ.10"))
  expect_no_error(resp_quantiles(resp, metric = "IA.HQ.10"))
})

test_that("assay_summary == FALSE", {

  # No assay column in "resp" input
  resp <- list(
    "r1" = data.frame(sample = c(1, 2, 1, 2),
                      GCA.Eff = c(1, 2, 3, 4),
                      IA.Eff = c(5, 6, 7, 8),
                      GCA.HQ.10 = c(9, 10, 11, 12),
                      IA.HQ.10 = c(13, 14, 15, 16)))
  
  expect_no_error(resp_quantiles(resp))
  
  expect_error(resp_quantiles(resp, assays = "a1"),
               "No assay column found in response data")
  
  # Add assay column
  resp[["r1"]] <- cbind(data.frame(assay = c("a1", "a1", "a2", "a2")),
                        resp[[1]])
  
  expect_warning(resp_quantiles(resp),
                 "Multiple assays found, using first assay")
  
  expect_no_warning(resp_quantiles(resp, assays = "a1"))
  
  expect_error(resp_quantiles(resp, assays = "not found"),
               "No response data for given assays")
  
})

test_that("assay_summary == TRUE", {
  
  # No assay column in "resp" input
  resp <- list(
    "r1" = data.frame(sample = c(1, 2, 1, 2),
                      GCA.Eff = c(1, 2, 3, 4),
                      IA.Eff = c(5, 6, 7, 8),
                      GCA.HQ.10 = c(9, 10, 11, 12),
                      IA.HQ.10 = c(13, 14, 15, 16)))
  
  expect_error(resp_quantiles(resp, assay_summary = TRUE),
               "Multiple assays required when 'assay_summary' is TRUE")
  
  # Add assay column
  resp[["r1"]] <- cbind(data.frame(assay = c("a1", "a1", "a2", "a2")),
                        resp[[1]])
  
  # No error, but warning for <= 10 assays
  expect_warning(
    out <- resp_quantiles(resp, assay_summary = TRUE),
    "Consider using a larger number of assays for a more robust analysis"
  )
  expect_equal(as.data.frame(out),
               data.frame(id = "r1",
                          assay_quantile = 0.5,
                          metric = "GCA.Eff",
                          summary_quantile = 0.1,
                          value = as.numeric(quantile(c(1.5, 3.5), 0.1))))
  
  # Specify assay
  expect_warning(
    out <- resp_quantiles(resp, assay_summary = TRUE, assays = "a1"),
    "Consider using a larger number of assays for a more robust analysis"
  )
  expect_equal(as.data.frame(out),
               data.frame(id = "r1",
                          assay_quantile = 0.5,
                          metric = "GCA.Eff",
                          summary_quantile = 0.1,
                          value = 1.5))
  
  expect_error(resp_quantiles(resp, assay_summary = TRUE, assays = "not found"),
               "No response data for given assays")
})
