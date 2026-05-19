#' A Generalized concentration addition style solution
#'
#' Find the effective concentration of a mixture via an objective function
#' given the concentrations and inverse, based on a regular space AC50 and
#' concentrations.
#'
#' @param conc_mix Effective concentration of the mixture in regular space.
#' @param resp Individual chemical responses.
#' @param conc Individual chemical concentrations in regular space.
#' @param max Maximal (asymtotic) response.
#' @param AC50 Concentrations of half-maximal response.
#'
#' @returns Objective value.
#' @noRd
obj_ECx <- function(conc_mix, resp, conc, max, AC50) {
  x <- hill_conc(resp, max, AC50, 1)
  p <- conc / sum(conc)
  ECx.val <- sum(p * conc_mix / x, na.rm = FALSE)
  (ECx.val - 1)^2
}
