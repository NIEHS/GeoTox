#' Objective function for 2-parameter Hill model
#'
#' @param p parameters c("tp", "log10_ga", "err")
#' @param log10_conc base-10 log scale concentration
#' @param resp response
#'
#' @return value
obj_hill_2par <- function (p, log10_conc, resp) {

  tp       <- p[1]  # top asymptote
  log10_ga <- p[2]  # log10(AC50)
  err      <- p[3]  # error term

  y <- tp / (1 + 10^(log10_ga - log10_conc))

  sum(stats::dt((resp - y) / exp(err), df = 3, log = TRUE) - err)
}
