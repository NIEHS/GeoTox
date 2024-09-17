#' Generalized concentration addition objective function
#'
#' @description
#' Use to find the optimal efficacy value based on a regular space AC50 and
#' concentrations.
#'
#' @param ln_resp natural log of individual chemical responses
#' @param conc individual chemical concentrations in regular space
#' @param max maximal (asymtotic) responses
#' @param AC50 concentrations of half-maximal response
#'
#' @return objective value
obj_GCA <- function(ln_resp, conc, max, AC50) {
  # Solving for the efficacy on the natural log-scale. This allows for
  # better precision in the low values, e.g. 1 x 10-5
  x <- hill_conc(exp(ln_resp), max, AC50, 1)
  gca.val <- sum(conc / x, na.rm = FALSE)
  (gca.val - 1)^2
}
