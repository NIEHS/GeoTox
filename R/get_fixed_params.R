#' Get C_ss Data for Fixed C_ss Generation Parameters
#'
#' @param simulated_css list of pre-generated C_ss data, for details see:
#' \code{vignette("package_data", package = "GeoTox")}.
#' @param age list of atomic vectors containing ages.
#'
#' @return list of matrices containing C_ss values.
#' @export
#' 
#' @examples
#' get_fixed_params(simulated_css = geo_tox_data$simulated_css,
#'                  age = list(c(25, 35, 55), c(15, 60)))
get_fixed_params <- function(simulated_css, age) {
  
  if (!is.list(age)) age <- list(age)
  
  lapply(age, function(x) {
    # Sample from "Normal" weight css values of median age group
    median_age <- stats::median(x)
    do.call(cbind, lapply(simulated_css, function(df) {
      css <- df |> 
        dplyr::filter(.data$weight == "Normal", .data$age_min <= median_age) |> 
        dplyr::arrange(.data$age_min) |> 
        dplyr::slice_tail(n = 1) |> 
        dplyr::pull(css) |> 
        unlist()
      sample(css, length(x), replace = TRUE)
    }))
  })
}
