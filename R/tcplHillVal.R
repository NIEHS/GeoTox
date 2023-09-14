#' TCPL Hill model
#'
#' @description
#' Calculate the efficacy for a given concentration in regular space
#'
#' @param c concentration in regular space
#' @param tp top asymptote
#' @param ga AC50
#' @param gw hill coefficient
#' @param bt bottom asymptote
#'
#' @return response value
tcplHillVal <- function(c, tp, ga, gw, bt = 0) {

  bt + (tp - bt) / (1 + (ga / c)^gw)

}
