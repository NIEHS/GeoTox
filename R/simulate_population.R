#' Simulate population data
#' 
#' @description
#' Simulate population data for given input fields
#'
#' @param x GeoTox object.
#' @param age input `x` to function [simulate_age]. After simulating ages, the
#' inhalation rate is subsequently calculated using [simulate_inhalation_rate].
#' @param obesity input `x` to function [simulate_obesity].
#' @param exposure input `x` to function [simulate_exposure].
#' @param simulated_css input `simulated_css` to functions [sample_Css] and
#' [get_fixed_css].
#' @param ... additional arguments passed to other functions. See details.
#'
#' @details
#' Additional parameters include `n` for sample size,
#' `IR_params` for [simulate_inhalation_rate],
#' `obes_prev`, `obes_sd`, and `obes_label` for [simulate_obesity],
#' and `expos_mean`, `expos_sd`, and `expos_label` for [simulate_exposure].
#' 
#' @return The same object with simulated fields added.
#' @export
#'
#' @examples
#' # For information about geo_tox_data, see:
#' # vignette("package_data", package = "GeoTox")
#' 
#' geoTox <- GeoTox() |> 
#'   simulate_population(age = split(geo_tox_data$age, ~FIPS)[1:5],
#'                       obesity = geo_tox_data$obesity[1:5, ],
#'                       exposure = split(geo_tox_data$exposure, ~FIPS)[1:5],
#'                       simulated_css = geo_tox_data$simulated_css,
#'                       n = 10)
#' @importFrom rlang .env
simulate_population <- function(x, age = NULL, obesity = NULL, exposure = NULL,
                                simulated_css = NULL, ...) {
  
  # Update parameters
  dots <- list(...)
  x$par$n <- dots$n %||% x$par$n
  x$par$IR_params <- dots$IR_params %||% x$par$IR_params
  x$par$obesity$obes_prev  <- dots$obes_prev   %||% x$par$obesity$obes_prev
  x$par$obesity$obes_sd    <- dots$obes_sd     %||% x$par$obesity$obes_sd
  x$par$obesity$obes_label <- dots$obes_label  %||% x$par$obesity$obes_label
  x$par$exposure$expos_mean  <- dots$expos_mean  %||% x$par$exposure$expos_mean
  x$par$exposure$expos_sd    <- dots$expos_sd    %||% x$par$exposure$expos_sd
  x$par$exposure$expos_label <- dots$expos_label %||% x$par$exposure$expos_label
  
  # Age
  if (!is.null(age)) {
    x$age <- simulate_age(age, n = x$par$n)
    # Clear downstream fields
    if (!is.null(x$C_ss) & is.null(simulated_css)) {
      warning("Clearing 'C_ss' and 'css_sensitivity' fields")
      x$C_ss <- NULL
    }
  }
  
  # Inhalation rate
  if (!is.null(age) | !is.null(dots$IR_params)) {
    x$IR <- simulate_inhalation_rate(x$age, IR_params = x$par$IR_params)
  }
  
  # Obesity status
  if (!is.null(obesity)) {
    x$obesity <- simulate_obesity(x          = obesity,
                                  obes_prev  = x$par$obesity$obes_prev,
                                  obes_sd    = x$par$obesity$obes_sd,
                                  obes_label = x$par$obesity$obes_label,
                                  n          = x$par$n)
    # Clear downstream fields
    if (!is.null(x$C_ss) & is.null(simulated_css)) {
      warning("Clearing 'C_ss' and 'css_sensitivity' fields")
      x$C_ss <- NULL
    }
  }
  
  # External exposure concentration
  if (!is.null(exposure)) {
    x$exposure <- exposure
    x$C_ext <- simulate_exposure(x           = x$exposure,
                                 expos_mean  = x$par$exposure$expos_mean,
                                 expos_sd    = x$par$exposure$expos_sd,
                                 expos_label = x$par$exposure$expos_label,
                                 n           = x$par$n)
  }
  
  # Sample from pre-simulated steady-state plasma concentration data
  if (!is.null(simulated_css)) {
    x$C_ss <- sample_Css(simulated_css, age = x$age, obesity = x$obesity)
    
    # Get values for sensitivity analysis
    x$css_sensitivity <- get_fixed_css(simulated_css, age = x$age,
                                       obesity = x$obesity, C_ss = x$C_ss)
  }
  
  x
}