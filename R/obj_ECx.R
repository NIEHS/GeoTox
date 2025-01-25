#' A Generalized concentration addition style solution
#'
#' @description
#' Find the effective concentration of a mixture via an objective function
#' given the concentrations and inverse, based on a regular space AC50 and
#' concentrations.
#'
#' @param conc_mix effective concentration of the mixture in regular space
#' @param resp individual chemical responses
#' @param conc individual chemical concentrations in regular space
#' @param max maximal (asymtotic) response
#' @param AC50 concentrations of half-maximal response
#'
#' @keywords internal
#' @noRd
#'
#' @return objective value
obj_ECx <- function(conc_mix, resp, conc, max, AC50) {
  x <- hill_conc(resp, max, AC50, 1)
  p <- conc / sum(conc)
  ECx.val <- sum(p * conc_mix / x, na.rm = FALSE)
  (ECx.val - 1)^2
}
