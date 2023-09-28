#' Calculate internal chemical dose
#'
#' @description
#' TODO A short description...
#'
#' @param C_ext ambient chemical concentration in \eqn{\frac{mg}{m^3}}
#' @param IR inhalation rate in \eqn{\frac{m^3}{day}}
#' @param time total time in \eqn{days}
#' @param BW body weight in \eqn{kg}
#'
#' @details
#' TODO Additional details...
#' \deqn{D_{int} = \frac{C_{ext} \,\times\, IR \,\times\, time}{BW}}
#'
#' @return internal chemical dose in \eqn{\frac{mg}{kg}}
#' @export
calc_internal_dose <- function(C_ext, IR, time = 1, BW = 1) {
  # TODO How to handle inputs with different units?
  # e.g. simulated inhalation rate is in m^3/(day * kg), so BW isn't needed
  # TODO paper states t = 365 in section 2.3, also states that C_ss achieved
  # in 1 day and repeated exposure accumulates additively. Computation done
  # with t = 1, is that correct?
  C_ext * IR * time / BW
}
