#' Independent action prediction from the tcpl Hill function
#'
#' @param Ci individual chemical concentrations in regular space
#' @param tp top asymptotes
#' @param AC50 AC50
#' @param Emax maximum mixture response
#' @param slope hill coefficient
#'
#' @return response value
calc_independent_action <- function(Ci, tp, AC50, Emax, slope = 1) {
  E <- tcplHillVal(Ci, tp, AC50, slope)
  Emax*(1 - prod((1 - E/Emax)))
}
