#' Calculate internal chemical dose
#'
#' @description
#' Estimate the internal dose from inhalation of a chemical given inhalation
#' rate, time, and body weight
#'
#' @param C_ext ambient chemical concentration in \eqn{\frac{mg}{m^3}}
#' @param IR inhalation rate in \eqn{\frac{m^3}{day}}
#' @param time total time in \eqn{days}
#' @param BW body weight in \eqn{kg}
#' @param scaling scaling factor encompassing any required unit adjustments
#'
#' @details
#' TODO Additional details...
#' \deqn{D_{int} = \frac{C_{ext} \,\times\, IR \,\times\, time}{BW}}
#'
#' @return internal chemical dose in \eqn{\frac{mg}{kg}}
#'
#' @examples
#' n_chem <- 3
#' n_sample <- 5
#'
#' # Single population
#' C_ext <- matrix(runif(n_sample * n_chem), ncol = n_chem)
#' IR <- runif(n_sample)
#' calc_internal_dose(C_ext, IR)
#'
#' # Multiple populations
#' C_ext <- list(
#'   "a" = matrix(runif(n_sample * n_chem), ncol = n_chem),
#'   "b" = matrix(runif(n_sample * n_chem), ncol = n_chem)
#' )
#' IR <- list(runif(n_sample), runif(n_sample))
#' calc_internal_dose(C_ext, IR)
#'
#' @export
calc_internal_dose <- function(C_ext, IR, time = 1, BW = 1, scaling = 1) {
  # TODO How to handle inputs with different units?
  # e.g. simulated inhalation rate is in m^3/(day * kg), so BW isn't needed
  # TODO paper states t = 365 in section 2.3, also states that C_ss achieved
  # in 1 day and repeated exposure accumulates additively. Computation done
  # with t = 1, is that correct?

  if (methods::is(C_ext, "matrix")) {
    .calc_internal_dose(C_ext, IR, time, BW, scaling)
  } else {
    mapply(.calc_internal_dose, C_ext, IR, time, BW, scaling, SIMPLIFY = FALSE)
  }
}

.calc_internal_dose <- function(C_ext, IR, time, BW, scaling) {
  C_ext * IR * time / BW * scaling
}
