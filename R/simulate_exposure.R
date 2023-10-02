#' Simulate external exposure
#'
#' @param mean array of concentration mean values.
#' @param sd array of concentration standard deviations, leave as 0 for fixed
#' external concentrations.
#' @param n simulated sample size.
#'
#' @return matrix \eqn{_{n \,\times\, length(mean)}} of inhalation rates.
#' @export
#'
#' @examples
#' simulate_exposure(mean = 1:3, n = 10)
#' simulate_exposure(mean = c(1, 10, 100), sd = c(0, 1, 5), n = 10)
simulate_exposure <- function(mean, sd = 0, n = 1e3) {
  if (length(mean) == 0) {
    matrix(0, nrow = n, ncol = 0)
  } else if (length(sd) == 1 && sd == 0) {
    t(replicate(n, mean))
  } else {
    mapply(
      function(mean, sd, n) {
        if (mean == 0) {
          rep(0, n)
        } else if (mean > 0 & is.na(sd)) {
          rep(mean, n)
        } else {
          truncnorm::rtruncnorm(
            n, a = 0, b = Inf, mean = mean, sd = sd
          )
        }
      },
      mean = mean,
      sd = sd,
      n = n
    )
  }
}
