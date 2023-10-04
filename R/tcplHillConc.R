#' TCPL Hill model
#'
#' @description
#' Calculate the concentration in regular space for a given value
#'
#' @param resp response value
#' @param tp top asymptote
#' @param ga AC50
#' @param gw hill coefficient
#'
#' @return concentration in regular space
tcplHillConc <- function(resp, tp, ga, gw) {

  # TODO add "bt" as input to match tcplHillVal.R?
  # ga * ((tp - bt) / (resp - bt) - 1)^(-1 / gw)

  # TODO add checks so bt < resp < tp?

  ga * (tp / resp - 1)^(-1 / gw)

}
