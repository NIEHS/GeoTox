#' Calculate response
#'
#' @description Calculate mixture response for GeoTox population data
#'
#' @param x GeoTox object
#' @param hill_params output from [fit_hill]
#' @param ... additional arguments passed to other functions
#'
#' @return The same object with additional fields added or updated
#' @export
#'
#' @seealso [fit_hill], [calc_internal_dose], [calc_invitro_concentration],
#' [calc_concentration_response]
#' @examples
#' x <- simulate_population(
#'   GeoTox(),
#'   age = split(geo_tox_data$age, ~FIPS)[1:5],
#'   obesity = geo_tox_data$obesity[1:5, ],
#'   exposure = split(geo_tox_data$exposure, ~FIPS)[1:5],
#'   simulated_css = geo_tox_data$simulated_css,
#'   n = 10
#' )
#' hill_params <- fit_hill(split(geo_tox_data$dose_response, ~casn))
#' x <- calculate_response(x, hill_params)
calculate_response <- function(x, hill_params, ...) {
  
  dots <- list(...)
  
  x$inputs$hill_params <- hill_params
  
  # Internal dose
  if (is.null(x$IR) | is.null(x$C_ext)) {
    stop("GeoTox object must contain 'IR' and 'C_ext' fields", call. = FALSE)
  }
  x$inputs$D_int <- list(
    time    = dots$time    %||% 1,
    BW      = dots$BW      %||% 1,
    scaling = dots$scaling %||% 1
  )
  x$D_int <- calc_internal_dose(x$C_ext,
                                x$IR,
                                time    = x$inputs$D_int$time,
                                BW      = x$inputs$D_int$BW,
                                scaling = x$inputs$D_int$scaling)
  
  # in vitro concentration
  if (is.null(x$C_ss)) {
    stop("GeoTox object must contain 'C_ss' field", call. = FALSE)
  }
  x$C_invitro <- calc_invitro_concentration(x$D_int, x$C_ss)
  
  # Concentration response
  x$inputs$resp <- list(
    tp_b_mult = dots$tp_b_mult %||% 1.5
  )
  x$resp <- calc_concentration_response(x$C_invitro,
                                        x$inputs$hill_params,
                                        tp_b_mult = x$inputs$resp$tp_b_mult,
                                        fixed     = FALSE)
  
  x
}
