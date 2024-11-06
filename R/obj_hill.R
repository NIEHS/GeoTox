#' Objective function for 2- or 3-parameter Hill model
#'
#' @param par parameters
#' @param log10_conc base-10 log scale concentration
#' @param resp response
#'
#' @keywords internal
#'
#' @return value of the objective function
obj_hill <- function(par, log10_conc, resp) {

  max        <- par[1]  # maximal response
  log10_AC50 <- par[2]  # log10(concentration of half-maximal response)
  if (length(par) == 3) {
    n        <- 1       # Hill coefficient (slope)
    err      <- par[3]
  } else {
    n        <- par[3]
    err      <- par[4]
  }

  y <- max / (1 + 10^(n * (log10_AC50 - log10_conc)))

  sum(stats::dt((resp - y) / exp(err), df = length(par), log = TRUE) - err)
}
