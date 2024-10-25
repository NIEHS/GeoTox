#' Calculate \emph{in vitro} concentration
#'
#' @description
#' Estimate the \emph{in vitro} equivalent plasma concentration given internal
#' chemical dose and steady-state plasma concentration.
#'
#' @param D_int internal chemical dose in \eqn{\frac{mg}{kg}}
#' @param C_ss steady-state plasma concentration in \eqn{\frac{\mu M}{mg / kg}}
#'
#' @details
#' Input `D_int` must be a matrix or list of matrices. Input `C_ss` must be a
#' numeric atomic vector or matrix, or a list of those types.
#' 
#' The \emph{in vitro} equivalent plasma concentration is calculated as:
#' \deqn{C_{plasma} = C_{ss} \times D_{int}}
#'
#' @return list of matrices containing concentrations in \eqn{\mu M}
#' 
#' @examples
#' # Single population
#' D_int <- matrix(1:15, ncol = 3)
#' C_ss <- 1:5
#' calc_invitro_concentration(D_int, C_ss)
#'
#' # Multiple populations
#' D_int <- list(
#'   "a" = matrix(1:15 / 10, ncol = 3),
#'   "b" = matrix(1:8, ncol = 2)
#' )
#' C_ss <- list(1:5, 1:4 / 2)
#' calc_invitro_concentration(D_int, C_ss)
#'
#' @export
calc_invitro_concentration <- function(D_int, C_ss = NULL) {

  D_int_err <- FALSE
  if (inherits(D_int, "matrix")) {
    D_int <- list(D_int)
  } else if (inherits(D_int, "list")) {
    if (!all(sapply(D_int, inherits, "matrix"))) {
      D_int_err <- TRUE
    }
  } else {
    D_int_err <- TRUE
  }
  if (D_int_err) {
    stop("`D_int` must be a matrix or a list of matrices",
         call. = FALSE)
  }

  if (is.null(C_ss)) {
    stop("real-time computation of C_ss values has not been implemented")
  }
  
  C_ss_err <- FALSE
  if (inherits(C_ss, c("matrix", "numeric", "integer"))) {
    C_ss <- list(C_ss)
  } else if (inherits(C_ss, "list")) {
    if (!all(sapply(C_ss, inherits, c("matrix", "numeric", "integer")))) {
      C_ss_err <- TRUE
    }
  } else {
    C_ss_err <- TRUE
  }
  if (C_ss_err) {
    stop("`C_ss` must be a matrix or numeric atomic vector, ",
         "or a list of those types",
         call. = FALSE)
  }
  
  mapply(.calc_invitro_concentration, D_int, C_ss, SIMPLIFY = FALSE)
}

.calc_invitro_concentration <- function(D_int, C_ss) {
  D_int * C_ss
}
