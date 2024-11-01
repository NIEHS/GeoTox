#' Calculate response
#'
#' @description Calculate mixture response for GeoTox population data
#'
#' @param x GeoTox object
#' @param ... additional arguments passed to other functions. See details.
#'
#' @details
#' Additional parameters include `time`, `BW`, and `scaling` for
#' [calc_internal_dose], and `max_mult` for [calc_concentration_response].
#'
#' @return The same object with additional fields added or updated
#' @export
#'
#' @seealso [calc_internal_dose], [calc_invitro_concentration],
#' [calc_concentration_response]
#' 
#' @examples
#' # Create GeoTox object
#' geoTox <- GeoTox()
#'
#' # The following fields are required inputs
#' geoTox$IR <- 2
#' geoTox$C_ext <- matrix(3)
#' geoTox$C_ss <- 5
#' geoTox$hill_params <- fit_hill(data.frame(logc = c(-1, 0, 1),
#'                                           resp = c(10, 5, 0)))
#'
#' # Calculate response
#' geoTox <- calculate_response(geoTox)
#' 
#' # The following fields will be computed
#' geoTox$D_int
#' geoTox$C_invitro
#' geoTox$resp
calculate_response <- function(x, ...) {
  
  # Update parameters
  dots <- list(...)
  x$par$internal_dose$time    <- dots$time    %||% x$par$internal_dose$time
  x$par$internal_dose$BW      <- dots$BW      %||% x$par$internal_dose$BW
  x$par$internal_dose$scaling <- dots$scaling %||% x$par$internal_dose$scaling
  x$par$resp$max_mult <- dots$max_mult %||% x$par$resp$max_mult

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
  if (is.null(x$hill_params)) {
    stop("GeoTox object must contain 'hill_params' field", call. = FALSE)
  }
  x$resp <- calc_concentration_response(x$C_invitro,
                                        x$hill_params,
                                        max_mult = x$par$resp$max_mult,
                                        fixed     = FALSE)
  
  x
}
