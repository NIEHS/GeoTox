#' A Generalized concentration addition style solution
#'
#' @description
#' Find the effective concentration of a mixture via an objective function
#' given the concentrations and inverse, based on a regular space AC50 and
#' concentrations.
#'
#' @param ECmix effective concentration of the mixture
#' @param E individual chemical responses
#' @param Ci individual chemical concentrations in regular space
#' @param tp top asymptotes
#' @param AC50 AC50s
#'
#' @return objective value
obj_ECx <- function(ECmix, E, Ci, tp, AC50) {
  ECi <- tcplHillConc(E, tp, AC50, rep(1, length(tp)))
  Pi <- Ci / sum(Ci)
  ECx.val <- sum(Pi * ECmix / ECi, na.rm = FALSE)
  (ECx.val - 1)^2
}
