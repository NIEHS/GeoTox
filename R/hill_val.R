#' Hill model response
#'
#' Calculate the response for a given concentration in regular space.
#'
#' This is a regular space version of
#' [tcpl::tcplHillVal()](https://cran.r-project.org/package=tcpl).
#'
#' The Hill model is defined as:
#' \deqn{resp = \frac{max}{1 + (\frac{AC50}{conc})^{n}}}
#'
#' @param conc Concentration in regular space.
#' @param max Maximal (asymptotic) response.
#' @param AC50 Concentration of half-maximal response.
#' @param n Hill coefficient (slope).
#'
#' @returns Response value.
#' @noRd
hill_val <- function(conc, max, AC50, n) {

  if (any(conc < 0) || any(AC50 < 0)) {
    stop("Input concentrations can't be negative.", call. = FALSE)
  }

  max / (1 + (AC50 / conc)^n)

}
