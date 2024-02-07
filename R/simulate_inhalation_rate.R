#' Simulate inhalation rates
#'
#' @param x array or list of arrays containing ages.
#' @param params (optional) data frame with columns "age", "mean" and "sd". See
#' details for more information.
#'
#' @details
#' The age column of the optional `params` data frame should be in ascending
#' order and represent the lower value of age groups for the corresponding mean
#' and sd values. When not provided, the default values will come from Table 6.7
#' of EPA's 2011 Exposure Factors Handbook using the mean of male and female
#' values.
#'
#' @return Array or list of arrays containing inhalation rates.
#'
#' @examples
#' # Single array
#' ages <- sample(1:100, 6, replace = TRUE)
#' simulate_inhalation_rate(ages)
#'
#' # List of arrays
#' ages <- list(
#'   sample(1:100, 5, replace = TRUE),
#'   sample(1:100, 3, replace = TRUE)
#' )
#' simulate_inhalation_rate(ages)
#'
#' @export
simulate_inhalation_rate <- function(x, params = NULL) {

  if (is.null(params)) {
    # Data comes from https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf
    # Table 6.7 Distribution percentiles of physiological daily inhalation rates
    # per unit body weight (m3/kg-day) for free living normal weight males and
    # females aged 2 months to 96 years
    params <- as.data.frame(rbind(
      c( 0, 0.495, 0.08, 0.48, 0.075),
      c( 1, 0.48,  0.06, 0.45, 0.08),
      c( 2, 0.44,  0.04, 0.44, 0.07),
      c( 5, 0.42,  0.05, 0.40, 0.05),
      c( 7, 0.37,  0.06, 0.35, 0.06),
      c(11, 0.30,  0.05, 0.27, 0.05),
      c(23, 0.25,  0.04, 0.23, 0.04),
      c(30, 0.24,  0.03, 0.24, 0.04),
      c(40, 0.23,  0.04, 0.21, 0.04),
      c(65, 0.19,  0.03, 0.17, 0.04)
    ))
    names(params) <- c("age", "male.mean", "male.sd", "female.mean", "female.sd")

    params$mean = rowMeans(params[, c("male.mean", "female.mean")])
    params$sd   = rowMeans(params[, c("male.sd", "female.sd")])
  } else {
    if (!all(c("age", "mean", "sd") %in% names(params))) {
      stop("params must contain columns \"age\", \"mean\" and \"sd\"")
    }
  }

  if (methods::is(x, "list")) {
    lapply(x, function(y) .simulate_inhalation_rate(y, params))
  } else {
    .simulate_inhalation_rate(x, params)
  }
}

.simulate_inhalation_rate <- function(x, params) {

  out <- rep(NA, length(x))

  age_idx <- is.numeric(x) & (x >= 0 & x < 100)

  if (any(age_idx)) {
    param_idx <- sapply(
      x[age_idx],
      function(age) max(which(age >= params$age))
    )
    out[age_idx] <- truncnorm::rtruncnorm(
      1, 0, Inf, params$mean[param_idx], params$sd[param_idx]
    )
  }

  out
}
