#' Hill model concentration
#'
#' @description
#' Calculate the concentration in regular space for a given response value.
#'
#' @param resp response value
#' @param max maximal (asymtotic) response
#' @param AC50 concentration of half-maximal response
#' @param n Hill coefficient (slope)
#'
#' @keywords internal
#' @noRd
#'
#' @return concentration in regular space
#' 
#' @details
#' This is a regular space version of
#' [tcpl::tcplHillConc()](https://cran.r-project.org/package=tcpl).
#' 
#' The concentration is computed as:
#' \deqn{conc = AC50 * (\frac{max}{resp} - 1)^{-1 / n}}
#' 
#' @seealso \code{\link{hill_val}}
hill_conc <- function(resp, max, AC50, n) {
  
  if (any(AC50 < 0)) {
    stop("Input concentration can't be negative.", call. = FALSE)
  }
  if (any((resp >= 0) != (max >= 0))) {
    stop("'resp' and 'max' must have the same sign.", call. = FALSE)
  }
  
  AC50 * (max / resp - 1)^(-1 / n)
  
}
