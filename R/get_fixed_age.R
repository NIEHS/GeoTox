#' Get C_ss Data for Fixed Age
#'
#' @param simulated_css list of pre-generated C_ss data, for details see:
#' \code{vignette("package_data", package = "GeoTox")}.
#' @param age list of atomic vectors containing ages.
#'
#' @return list of matrices containing median C_ss values.
#' @importFrom rlang .data
#' @export
#' 
#' @examples
#' get_fixed_age(simulated_css = geo_tox_data$simulated_css,
#'               age = list(c(25, 35, 55), c(15, 60)))
get_fixed_age <- function(simulated_css, age) {
  
  if (!is.list(age)) age <- list(age)
  
  lapply(age, function(x) {
    do.call(cbind, lapply(simulated_css, function(df) {
      df <- df |> 
        dplyr::distinct(.data$age_min, .data$age_median_css) |> 
        dplyr::arrange(.data$age_min)
      idx <- sapply(
        x,
        function(y) utils::tail(which(y >= df$age_min), 1)
      )
      df$age_median_css[idx]
    }))
  })
}
