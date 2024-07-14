#' Get C_ss Data for Fixed Obesity Status
#'
#' @param simulated_css list of pre-generated C_ss data, for details see:
#' \code{vignette("package_data", package = "GeoTox")}.
#' @param obesity list of atomic vectors containing obesity status.
#'
#' @return list of matrices containing median C_ss values.
#' @importFrom rlang .data
#' @export
#' 
#' @examples
#' get_fixed_obesity(simulated_css = geo_tox_data$simulated_css,
#'                   obesity = list(c("Obese", "Normal", "Obese"),
#'                                  c("Normal", "Normal")))
get_fixed_obesity <- function(simulated_css, obesity) {
  
  if (!is.list(obesity)) obesity <- list(obesity)
  
  lapply(obesity, function(x) {
    do.call(cbind, lapply(simulated_css, function(df) {
      df <- df |> dplyr::distinct(.data$weight, .data$weight_median_css)
      df$weight_median_css[match(x, df$weight)]
    }))
  })
}
