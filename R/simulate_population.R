#' Simulate population data
#' 
#' @description Simulate population data for given input fields
#'
#' @param x GeoTox object.
#' @param age input `x` to function [simulate_age]. After simulating ages, the
#' inhalation rate is subsequently calculated using [simulate_inhalation_rate].
#' @param obesity input `x` to function [simulate_obesity].
#' @param exposure input `x` to function [simulate_exposure].
#' @param simulated_css input `simulated_css` to function [sample_Css].
#' @param n simulated sample size.
#' @param ... additional arguments passed to other functions.
#'
#' @return The same object with simulated fields added.
#' @export
#'
#' @seealso [simulate_age], [simulate_inhalation_rate], [simulate_obesity],
#' [simulate_exposure], [sample_Css]
#' @examples
#' x <- simulate_population(
#'   GeoTox(),
#'   age = split(geo_tox_data$age, ~FIPS)[1:5],
#'   obesity = geo_tox_data$obesity[1:5, ],
#'   exposure = split(geo_tox_data$exposure, ~FIPS)[1:5],
#'   simulated_css = geo_tox_data$simulated_css,
#'   n = 10
#' )
#' @importFrom rlang .env
simulate_population <- function(x, age = NULL, obesity = NULL, exposure = NULL,
                                simulated_css = NULL, n = 1e3, ...) {
  
  dots <- list(...)
  
  if (is.null(x$inputs)) {
    x$inputs <- list(n = n)
  }
  
  # Age and inhalation rate
  if (!is.null(age)) {
    x$age <- simulate_age(x = age, n = n)
    x$inputs$IR_params <- dots$IR_params
    x$IR <- simulate_inhalation_rate(x$age,
                                     IR_params = x$inputs$IR_params)
  }
  
  # Obesity status
  if (!is.null(obesity)) {
    obes_prev  <- dots$obes_prev  %||% "OBESITY_CrudePrev"
    obes_sd    <- dots$obes_sd    %||% "OBESITY_SD"
    obes_label <- dots$obes_label %||% "FIPS"
    
    x$obesity <- simulate_obesity(x          = obesity,
                                  obes_prev  = obes_prev,
                                  obes_sd    = obes_sd,
                                  obes_label = obes_label,
                                  n          = x$inputs$n)
  }
  
  # External exposure concentration
  if (!is.null(exposure)) {
    x$inputs$exposure <- list(
      x           = exposure,
      expos_mean  = dots$expos_mean  %||% "mean",
      expos_sd    = dots$expos_sd    %||% "sd",
      expos_label = dots$expos_label %||% "casn"
    )
    x$C_ext <- simulate_exposure(x           = x$inputs$exposure$x,
                                 expos_mean  = x$inputs$exposure$expos_mean,
                                 expos_sd    = x$inputs$exposure$expos_sd,
                                 expos_label = x$inputs$exposure$expos_label,
                                 n           = x$inputs$n)
  }
  
  # Sample from pre-simulated steady-state plasma concentration data
  if (!is.null(simulated_css)) {
    # Check names
    if (length(x$age) != length(x$obesity)) {
      stop("Length of 'age' and 'obesity' fields must be equal", call. = FALSE)
    }
    if (length(x$age) == 0) {
      stop("'age' and 'obesity' data must be simulated first", call. = FALSE)
    }
    if (length(x$age) > 1 & !all(names(x$age) == names(x$obesity))) {
      stop("Names of 'age' and 'obesity' fields must be equal", call. = FALSE)
    }
    x$C_ss <- sample_Css(simulated_css, x$age, x$obesity)
  }
  
  # Get values for sensitivity analysis
  
  if (is.null(x$css_sensitivity)) {
    x$css_sensitivity <- list()
  }
  
  if (!is.null(age) | !is.null(simulated_css)) {
    x$css_sensitivity$age <- get_fixed_age(x$age, simulated_css)
    x$css_sensitivity$params <- get_fixed_params(x$age, simulated_css)
  }
  
  if (!is.null(obesity) | !is.null(simulated_css)) {
    x$css_sensitivity$obesity <- get_fixed_obesity(x$obesity, simulated_css)
  }
  
  if (!is.null(simulated_css)) {
    x$css_sensitivity$other <- get_fixed_other(x$C_ss)
  }
  
  x
}