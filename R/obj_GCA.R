#' Generalized concentration addition objective function
#'
#' @description
#' Use to find the optimal efficacy value, E, based on a regular space AC50 and
#' concentrations.
#'
#' @param effic natural log of individual chemical responses
#' @param Ci individual chemical concentrations in regular space
#' @param tp top asymptote
#' @param AC50 AC50
#'
#' @return objective value
obj_GCA <- function(effic, Ci, tp, AC50) {
  # Solving for the efficacy on the natural log-scale. This allows for
  # better precision in the low values, e.g. 1 x 10-5
  E <- exp(effic)
  ECi <- tcplHillConc(E, tp, AC50, rep(1,length(tp)))
  gca.val <- sum(Ci / ECi, na.rm = FALSE)
  (gca.val - 1)^2
}
