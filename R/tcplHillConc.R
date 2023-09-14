#' TCPL Hill model
#'
#' @description
#' Calculate the concentration in regular space for a given value
#'
#' @param val response value
#' @param tp top asymptote
#' @param ga AC50
#' @param gw hill coefficient
#'
#' @return concentration in regular space
tcplHillConc <- function(val, tp, ga, gw) {

  ga * (tp/val - 1)^(-1/gw)

}
