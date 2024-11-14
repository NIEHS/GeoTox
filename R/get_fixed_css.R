#' Get Fixed `C_ss` Data
#' 
#' @description
#' Get `C_ss` values for use in [sensitivity_analysis] and [compute_sensitivity].
#'
#' @param simulated_css list of pre-generated `C_ss` data, for details see:
#' \code{vignette("package_data", package = "GeoTox")}.
#' @param age list of atomic vectors containing ages.
#' @param obesity list of atomic vectors containing obesity status.
#' @param C_ss list of matrices containing `C_ss` values.
#'
#' @return list of matrices or atomic vectors containing `C_ss` values.
#' @export
#' 
#' @examples
#' # Define inputs
#' age <- list(c(25, 35, 55),
#'             c(15, 60))
#' obesity <- list(c("Obese", "Normal", "Obese"),
#'                 c("Normal", "Normal"))
#' C_ss <- sample_Css(simulated_css = geo_tox_data$simulated_css,
#'                    age = age,
#'                    obesity = obesity)
#' 
#' # Get fixed C_ss data
#' get_fixed_css(simulated_css = geo_tox_data$simulated_css,
#'               age = age,
#'               obesity = obesity,
#'               C_ss = C_ss)
get_fixed_css <- function(simulated_css, age, obesity, C_ss) {
  
  if (.check_lengths(age, obesity)) {
    stop("Names and lengths of 'age' and 'obesity' fields must be equal",
         call. = FALSE)
  }
  
  list(
    age     = get_fixed_age(simulated_css, age),
    params  = get_fixed_params(simulated_css, age),
    obesity = get_fixed_obesity(simulated_css, obesity),
    other   = get_fixed_other(C_ss)
  )
}
