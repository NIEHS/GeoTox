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
#' @return response value
#' @export
#' 
#' @details
#' This is a regular space version of
#' [tcpl::tcplHillVal()](https://cran.r-project.org/package=tcpl).
#' 
#' The Hill model is defined as:
#' \deqn{resp = \frac{max}{1 + (\frac{AC50}{conc})^{n}}}
#' 
#' @seealso \code{\link{hill_conc}}
#' 
#' @examples
#' hill_val(c(0.0025, 0.01, 0.03), 1, 0.01, 1)
#' hill_val(c(0.05, 0.01, 0.003), 1, c(0.1, 0.01, 0.001), 2)
hill_val <- function(conc, max, AC50, n) {

  if (any(conc < 0) || any(AC50 < 0)) {
    stop("Input concentrations can't be negative.", call. = FALSE)
  }
  
  max / (1 + (AC50 / conc)^n)

}
