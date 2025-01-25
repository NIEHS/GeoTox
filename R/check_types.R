#' Check types
#'
#' @param x object to check
#' @param types allowed types
#' @param msg error message
#'
#' @keywords internal
#' @noRd
#'
#' @return list version of input
.check_types <- function(x, types, msg = "Incorrect type") {
  err <- FALSE
  if (inherits(x, types)) {
    x <- list(x)
  } else if (inherits(x, "list")) {
    if (!all(sapply(x, inherits, types))) {
      err <- TRUE
    }
  } else {
    err <- TRUE
  }
  if (err) stop(msg, call. = FALSE)
  x
}
