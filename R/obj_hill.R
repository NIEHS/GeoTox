#' Objective function for 2- or 3-parameter Hill model
#'
#' @param par parameters
#' @param log10_conc base-10 log scale concentration
#' @param resp response
#'
#' @return value
obj_hill <- function (par, log10_conc, resp) {

  tp       <- par[1]  # top asymptote
  log10_ga <- par[2]  # log10(AC50)
  if (length(par) == 3) {
    gw     <- 1     # slope
    err    <- par[3]
  } else {
    gw     <- par[3]
    err    <- par[4]
  }

  y <- tp / (1 + 10^(gw * (log10_ga - log10_conc)))

  sum(stats::dt((resp - y) / exp(err), df = length(par), log = TRUE) - err)
}
