#' Title
#'
#' @param x .
#' @param region .
#' @param group .
#'
#' @return .
#' @export
set_boundaries <- function(x, region = NULL, group = NULL, individuals = NULL) {
  x$boundaries <- list(
    region = region,
    group = group,
    individuals = individuals
  )
  x
}
