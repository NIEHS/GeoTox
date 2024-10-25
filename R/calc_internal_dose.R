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
#' Input `C_ext` must be a matrix or list of matrices. Input `IR` must be an
#' atomic vector or list of atomic vectors. The `time`, `BW` and `scaling`
#' arguments are scalars.
#' 
#' The internal dose is calculated as:
#' \deqn{D_{int} = \frac{C_{ext} \times IR \times time}{BW} \times scaling}
#'
#' @return list of matrices containing internal chemical doses in
#' \eqn{\frac{mg}{kg}}
#'
#' @examples
#' # Single population
#' C_ext <- matrix(1:15, ncol = 3)
#' IR <- 1:5
#' calc_internal_dose(C_ext, IR)
#'
#' # Multiple populations
#' C_ext <- list(
#'   "a" = matrix(1:15 / 10, ncol = 3),
#'   "b" = matrix(1:8, ncol = 2)
#' )
#' IR <- list(1:5, 1:4 / 2)
#' calc_internal_dose(C_ext, IR)
#'
#' @export
calc_internal_dose <- function(C_ext, IR, time = 1, BW = 1, scaling = 1) {
  
  C_ext <- .check_types(C_ext,
                        "matrix",
                        "`C_ext` must be a matrix or a list of matrices.")
  
  IR <- .check_types(IR,
                     c("numeric", "integer"),
                     paste("`IR` must be a numeric atomic vector or a list of",
                           "atomic vectors."))

  mapply(.calc_internal_dose, C_ext, IR, time, BW, scaling, SIMPLIFY = FALSE)
}

.calc_internal_dose <- function(C_ext, IR, time, BW, scaling) {
  C_ext * IR * time / BW * scaling
}
