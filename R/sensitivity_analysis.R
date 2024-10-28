#' Perform sensitivity analysis
#'
#' @param x GeoTox object.
#' @param max_mult numeric list of length 5 for each step of the sensitivity
#' analysis.
#' 
#' @details
#' This wrapper function will sequentially call the [compute_sensitivity]
#' function with inputs `age`, `obesity`, `css_params`, `fit_params`, and
#' `C_ext`. The results will be returned as a named list and stored in the
#' `sensitivity` field of the input GeoTox object.
#' 
#' Values of `NULL` in the `max_mult` input will use the default value stored
#' in the `GeoTox` object (`x$par$resp$max_mult`). When a `GeoTox` object is
#' created this is initialized at `1.5`, but can be changed via the
#' [calculate_response] function or directly in the object.
#' 
#' @seealso \code{\link{compute_sensitivity}}
#'
#' @return The same GeoTox object with added `sensitivity` field.
#' @export
sensitivity_analysis <- function(x,
                                 max_mult = list(NULL, NULL, NULL,
                                                 1.2, NULL)) {
  
  if (!inherits(x, "GeoTox")) {
    stop("Input 'x' must be a GeoTox object.", call. = FALSE)
  }
  if (length(max_mult) != 5) {
    stop("Input 'max_mult' must have a length of 5.", call. = FALSE)
  }
  
  x$sensitivity <- list(
    age        = compute_sensitivity(x,
                                     vary = "age",
                                     max_mult = max_mult[[1]]),
    obesity    = compute_sensitivity(x,
                                     vary = "obesity",
                                     max_mult = max_mult[[2]]),
    css_params = compute_sensitivity(x,
                                     vary = "css_params",
                                     max_mult = max_mult[[3]]),
    fit_params = compute_sensitivity(x,
                                     vary = "fit_params",
                                     max_mult = max_mult[[4]]),
    C_ext      = compute_sensitivity(x,
                                     vary = "C_ext",
                                     max_mult = max_mult[[5]])
  )
  
  x
}
