#' Calculate response
#'
#' @description Calculate mixture response for GeoTox population data
#'
#' @param x GeoTox object
#' @param ... additional arguments passed to other functions. See details.
#'
#' @details
#' Additional parameters include `time`, `BW`, and `scaling` for
#' [calc_internal_dose], and `tp_b_mult` for [calc_concentration_response].
#'
#' @return The same object with additional fields added or updated
#' @export
#'
#' @seealso [calc_internal_dose], [calc_invitro_concentration],
#' [calc_concentration_response]
calculate_response <- function(x, ...) {
  
  # Update parameters
  dots <- list(...)
  x$par$internal_dose$time    <- dots$time    %||% x$par$internal_dose$time
  x$par$internal_dose$BW      <- dots$BW      %||% x$par$internal_dose$BW
  x$par$internal_dose$scaling <- dots$scaling %||% x$par$internal_dose$scaling
  x$par$resp$tp_b_mult <- dots$tp_b_mult %||% x$par$resp$tp_b_mult

  # Internal dose
  if (is.null(x$IR) | is.null(x$C_ext)) {
    stop("GeoTox object must contain 'IR' and 'C_ext' fields", call. = FALSE)
  }
  x$D_int <- calc_internal_dose(x$C_ext,
                                x$IR,
                                time    = x$par$internal_dose$time,
                                BW      = x$par$internal_dose$BW,
                                scaling = x$par$internal_dose$scaling)
  
  # in vitro concentration
  if (is.null(x$C_ss)) {
    stop("GeoTox object must contain 'C_ss' field", call. = FALSE)
  }
  x$C_invitro <- calc_invitro_concentration(x$D_int, x$C_ss)
  
  # Concentration response
  if (is.null(x$C_ss)) {
    stop("GeoTox object must contain 'hill_params' field", call. = FALSE)
  }
  x$resp <- calc_concentration_response(x$C_invitro,
                                        x$hill_params,
                                        tp_b_mult = x$par$resp$tp_b_mult,
                                        fixed     = FALSE)
  
  x
}
