#' Simulate inhalation rates
#'
#' @param x numeric vector or list of numeric vectors containing ages.
#' @param IR_params (optional) data frame with columns "age", "mean" and "sd".
#' See details for more information.
#'
#' @details
#' The age column of the optional `IR_params` data frame should be in ascending
#' order and represent the lower value of age groups for the corresponding mean
#' and sd values. When not provided, the default values will come from Table 6.7
#' of EPA's 2011 Exposure Factors Handbook using the mean of male and female
#' values.
#'
#' @return List of numeric vectors containing inhalation rates.
#'
#' @examples
#' # Single numeric vector
#' ages <- sample(1:100, 6, replace = TRUE)
#' simulate_inhalation_rate(ages)
#'
#' # List of numeric vectors
#' ages <- list(
#'   sample(1:100, 5, replace = TRUE),
#'   sample(1:100, 3, replace = TRUE)
#' )
#' simulate_inhalation_rate(ages)
#'
#' # Custom IR_params
#' IR_params <- data.frame("age" = c(0, 20, 50),
#'                         "mean" = c(0.5, 0.3, 0.2),
#'                         "sd" = c(0.1, 0.06, 0.03))
#' simulate_inhalation_rate(c(15, 30, 65), IR_params)
#'
#' @export
simulate_inhalation_rate <- function(x, IR_params = NULL) {

  x <- .check_types(x,
                    c("integer", "numeric"),
                    "`x` must be a numeric vector or list of numeric vectors")
  
  if (is.null(IR_params)) {
    # Data comes from https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf
    # Table 6.7 Distribution percentiles of physiological daily inhalation rates
    # per unit body weight (m3/kg-day) for free living normal weight males and
    # females aged 2 months to 96 years
    IR_params <- tibble::tribble(
      ~age, ~male.mean, ~male.sd, ~female.mean, ~female.sd,
       0, 0.495, 0.08, 0.48, 0.075,
       1, 0.48,  0.06, 0.45, 0.08,
       2, 0.44,  0.04, 0.44, 0.07,
       5, 0.42,  0.05, 0.40, 0.05,
       7, 0.37,  0.06, 0.35, 0.06,
      11, 0.30,  0.05, 0.27, 0.05,
      23, 0.25,  0.04, 0.23, 0.04,
      30, 0.24,  0.03, 0.24, 0.04,
      40, 0.23,  0.04, 0.21, 0.04,
      65, 0.19,  0.03, 0.17, 0.04
    )
    IR_params$mean = rowMeans(IR_params[, c("male.mean", "female.mean")])
    IR_params$sd   = rowMeans(IR_params[, c("male.sd", "female.sd")])
  } else {
    if (!all(c("age", "mean", "sd") %in% names(IR_params))) {
      stop("`IR_params` must contain columns `age`, `mean` and `sd`")
    }
  }
  
  IR_params <- IR_params[order(IR_params$age), ]
  
  purrr::map(x, \(ages) .simulate_inhalation_rate(ages, IR_params))
}

.simulate_inhalation_rate <- function(x, IR_params) {

  out <- rep(NA, length(x))

  age_idx <- is.numeric(x) & (x >= IR_params$age[1] & x < 100)

  if (any(age_idx)) {
    param_idx <- sapply(
      x[age_idx],
      function(age) max(which(age >= IR_params$age))
    )
    out[age_idx] <- truncnorm::rtruncnorm(1,
                                          0,
                                          Inf,
                                          IR_params$mean[param_idx],
                                          IR_params$sd[param_idx])
  }

  out
}
