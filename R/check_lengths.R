#' Check for matching lengths
#'
#' @param x list of arrays
#' @param y list of arrays
#' 
#' @keywords internal
#' @noRd
#'
#' @return boolean, TRUE for error
.check_lengths <- function(x, y) {
  len_x <- lapply(x, length) |> unlist()
  len_y <- lapply(y, length) |> unlist()
  !identical(len_x, len_y)
}