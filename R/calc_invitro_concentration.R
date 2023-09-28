#' Calculate \emph{in vitro} concentration
#'
#' @description
#' TODO A short description...
#'
#' @param D_int internal chemical dose in \eqn{\frac{mg}{kg}}
#' @param C_ss steady-state plasma concentration in \eqn{\frac{\mu M}{mg / kg}}
#'
#' @details
#' TODO Additional details...
#' \deqn{C_{plasma} = C_{ss} \,\times\, D_{int}}
#'
#' TODO If C_ss is not provided, uses \emph{httk} to create...
#'
#' @return \emph{in vitro} equivalent plasma concentration in \eqn{\mu M}
#' @export
calc_invitro_concentration <- function(D_int, C_ss = NULL) {
  if (is.null(C_ss)) {
    # TODO add real-time computation of Css values
    stop("real-time computation of Css values has not been implemented")
  }
  # TODO the current C_ss data passed into this for step 01-Sensitivity.R
  # doesn't match the ages that were simulated?
  D_int * C_ss
}
