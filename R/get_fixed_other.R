#' Title
#'
#' @param C_ss .
#'
#' @return .
#' @export
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