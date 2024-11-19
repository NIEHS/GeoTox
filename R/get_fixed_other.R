#' Get median `C_ss` Values
#'
#' @param C_ss list of matrices containing `C_ss` data
#'
#' @return list of atomic vectors containing median `C_ss` values.
#' @export
#' 
#' @examples
#' # Generate input C_ss data
#' age <- list(c(25, 35, 55),
#'             c(15, 60))
#' obesity <- list(c("Obese", "Normal", "Obese"),
#'                 c("Normal", "Normal"))
#' C_ss <- sample_Css(simulated_css = geo_tox_data$simulated_css,
#'                    age = age,
#'                    obesity = obesity)
#' 
#' # Get median C_ss values
#' get_fixed_other(C_ss)
get_fixed_other <- function(C_ss) {
  
  for (i in 1:length(C_ss)) {
    # Mean impute for each chemical
    for (j in 1:ncol(C_ss[[i]])) {
      idx <- is.na(C_ss[[i]][, j])
      if (any(idx)) {
        C_ss[[i]][idx, j] <- mean(C_ss[[i]][!idx, j])
      }
    }
  }
  
  # Return overall median value
  lapply(
    C_ss,
    function(x) rep(stats::median(x), length.out = nrow(x))
  )
}
