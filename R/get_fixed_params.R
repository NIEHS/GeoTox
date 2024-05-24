#' Title
#'
#' @param age x
#' @param simulated_css x
#'
#' @return x
#' @importFrom rlang .data
#' @export
get_fixed_params <- function(age, simulated_css) {
  
  if (!is.list(age)) age <- list(age)
  
  lapply(age, function(x) {
    # Sample from "Normal" weight css values of median age group
    median_age <- stats::median(x)
    do.call(cbind, lapply(simulated_css, function(df) {
      css <- df |> 
        dplyr::filter(.data$weight == "Normal", .data$age_min <= median_age) |> 
        dplyr::slice_tail(n = 1) |> 
        dplyr::pull(css) |> 
        unlist()
      sample(css, length(x), replace = TRUE)
    }))
  })
}