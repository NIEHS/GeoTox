#' Hill model concentration
#'
#' Calculate the concentration in regular space for a given response value.
#'
#' This is a regular space version of
#' [tcpl::tcplHillConc()](https://cran.r-project.org/package=tcpl).
#'
#' The concentration is computed as:
#' \deqn{conc = AC50 * (\frac{max}{resp} - 1)^{-1 / n}}
#'
#' @param resp Response value.
#' @param max Maximal (asymptotic) response.
#' @param AC50 Concentration of half-maximal response.
#' @param n Hill coefficient (slope).
#'
#' @returns Concentration in regular space.
#' @noRd
hill_conc <- function(resp, max, AC50, n) {

  if (any(AC50 < 0)) {
    stop("Input concentration can't be negative.", call. = FALSE)
  }
  if (any((resp >= 0) != (max >= 0))) {
    stop("'resp' and 'max' must have the same sign.", call. = FALSE)
  }

  AC50 * (max / resp - 1)^(-1 / n)

}
