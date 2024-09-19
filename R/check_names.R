#' Check for names
#'
#' @param x object to check
#' @param names names to look for
#'
#' @return boolean, TRUE for error
#' @keywords internal
.check_names <- function(x, names) {
  if (inherits(x, "list")) {
    if (any(unlist(lapply(x, function(y) !all(names %in% names(y)))))) {
      return(TRUE)
    }
  } else {
    if (!all(names %in% names(x))) {
      return(TRUE)
    }
  }
  FALSE
}