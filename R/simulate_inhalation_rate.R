#' Simulate inhalation rates
#'
#' @param ages array of ages.
#'
#' @return array of inhalation rates.
#' @export
#'
#' @examples
#' simulate_inhalation_rate(c(1, 6, 20))
simulate_inhalation_rate <- function(ages) {

  # Data comes from https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf
  # Table 6.7 Distribution percentiles of physiological daily inhalation rates per unit
  # body weight (m3/kg-day) for free living normal weight males and females aged 2 months to 96 years
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

  ir <- rep(NA, length(ages))
  age_idx <- is.numeric(ages) & (ages >= 0 & ages < 100)
  if (any(age_idx)) {
    param_idx <- sapply(ages[age_idx], \(age) max(which(age >= params$age)))
    ir[age_idx] <- truncnorm::rtruncnorm(1, 0, Inf, params$mean[param_idx], params$sd[param_idx])
  }
  ir
}
