#' Hill model response
#'
#' @description
#' Calculate the response for a given concentration in regular space.
#'
#' @param conc concentration in regular space
#' @param max maximal (asymtotic) response
#' @param AC50 concentration of half-maximal response
#' @param n Hill coefficient (slope)
#'
#' @keywords internal
#'
#' @return response value
#' 
#' @details
#' This is a regular space version of
#' [tcpl::tcplHillVal()](https://cran.r-project.org/package=tcpl).
#' 
#' The Hill model is defined as:
#' \deqn{resp = \frac{max}{1 + (\frac{AC50}{conc})^{n}}}
#' 
#' @seealso \code{\link{hill_conc}}
hill_val <- function(conc, max, AC50, n) {

  if (any(conc < 0) || any(AC50 < 0)) {
    stop("Input concentrations can't be negative.", call. = FALSE)
  }
  
  max / (1 + (AC50 / conc)^n)

}
