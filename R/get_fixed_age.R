#' Title
#'
#' @param age x
#' @param simulated_css x
#'
#' @return x
#' @importFrom rlang .data
#' @export
get_fixed_age <- function(age, simulated_css) {
  
  if (!is.list(age)) age <- list(age)
  
  lapply(age, function(x) {
    do.call(cbind, lapply(simulated_css, function(df) {
      df <- df |> dplyr::distinct(.data$age_min, .data$age_median_css)
      idx <- sapply(
        x,
        function(y) utils::tail(which(y >= df$age_min), 1)
      )
      df$age_median_css[idx]
    }))
  })
}