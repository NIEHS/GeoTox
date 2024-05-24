#' Title
#'
#' @param obesity x
#' @param simulated_css x
#'
#' @return x
#' @importFrom rlang .data
#' @export
get_fixed_obesity <- function(obesity, simulated_css) {
  
  if (!is.list(obesity)) obesity <- list(obesity)
  
  lapply(obesity, function(x) {
    do.call(cbind, lapply(simulated_css, function(df) {
      df <- df |> dplyr::distinct(.data$weight, .data$weight_median_css)
      df$weight_median_css[match(x, df$weight)]
    }))
  })
}