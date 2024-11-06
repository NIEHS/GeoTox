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
#' # Use a subset of the package data for demonstration purposes
#' set.seed(2357)
#' n <- 10 # Population size
#' m <- 5 # Number of regions
#' idx <- if (m < 100) sample(1:100, m) else 1:100
#'
#' # Create GeoTox object and populate required fields
#' geoTox <- GeoTox() |>
#'   # Simulate populations for each region
#'   simulate_population(age = split(geo_tox_data$age, ~FIPS)[idx],
#'                       obesity = geo_tox_data$obesity[idx, ],
#'                       exposure = split(geo_tox_data$exposure, ~FIPS)[idx],
#'                       simulated_css = geo_tox_data$simulated_css,
#'                       n = n) |>
#'   # Estimated Hill parameters
#'   set_hill_params(geo_tox_data$dose_response |>
#'                     fit_hill(assay = "endp", chem = "casn") |>
#'                     dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed))
#'
#' # Response computations can now be done
#' geoTox <- geoTox |> calculate_response()
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
