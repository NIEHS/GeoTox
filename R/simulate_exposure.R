#' Simulate external exposure
#'
#' @param x data frame with a "concentration_mean" column.
#' @param n simulated sample size.
#'
#' @return matrix of inhalation rates.
#' @export
#'
#' @examples
#' x <- data.frame(concentration_mean = 1:3)
#' simulate_exposure(x, 10)
simulate_exposure <- function(x, n = 1e3) {
  # NOTE: This is written based only on 01-Sensitivity.R so far.
  #       If it doesn't become more complex later on, then the replication
  #       part can be removed.
  if (nrow(x) == 0) {
    matrix(0, nrow = n, ncol = 0)
  } else {
    t(replicate(n, x$concentration_mean))
  }
}
