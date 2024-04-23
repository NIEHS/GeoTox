#' Simulate population data
#' 
#' @description Simulate population data for given input fields
#'
#' @param x a GeoTox object
#' @param age input `x` to function [simulate_age]. After simulating ages, the
#' inhalation rate is subsequently calculated using [simulate_inhalation_rate].
#' @param obesity input `x` to function [simulate_obesity]
#' @param exposure input `x` to function [simulate_exposure]
#' @param simulated_css input `simulated_css` to function [sample_Css]
#' @param n simulated sample size
#' @param ... additional arguments passed to other functions
#'
#' @return The same object with simulated fields added
#' @export
#'
#' @seealso [simulate_age], [simulate_inhalation_rate], [simulate_obesity],
#' [simulate_exposure], [sample_Css]
#' @examples
#' x <- simulate(GeoTox(),
#'               age = split(geo_tox_data$age, ~FIPS)[1:5],
#'               obesity = geo_tox_data$obesity[1:5, ],
#'               exposure = split(geo_tox_data$exposure, ~FIPS)[1:5],
#'               simulated_css = geo_tox_data$simulated_css,
#'               n = 10)
simulate <- function(x, age = NULL, obesity = NULL, exposure = NULL,
                     simulated_css = NULL, n = 1e3, ...) {
  
  # Age and inhalation rate
  if (!is.null(age)) {
    x$age <- simulate_age(x = age, n = n)
    x$IR <- simulate_inhalation_rate(x$age, ...)
  }
  
  # Obesity status
  if (!is.null(obesity)) {
    x$obesity <- simulate_obesity(x = obesity, n = n, ...)
  }
  
  # External exposure concentration
  if (!is.null(exposure)) {
    x$C_ext <- simulate_exposure(x = exposure, n = n, ...)
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
    if (length(x$age) > 1 &
        !all(names(x$age) == names(x$obesity))) {
      stop("Names of 'age' and 'obesity' fields must be equal", call. = FALSE)
    }
    x$C_ss <- sample_Css(simulated_css, x$age, x$obesity)
  }
  
  x
}
