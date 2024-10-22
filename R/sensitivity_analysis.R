#' Perform sensitivity analysis
#'
#' @param x GeoTox object.
#' @param tp_b_mult numeric list of length 5 for each step of the sensitivity
#' analysis.
#' 
#' @details
#' This wrapper function will sequentially call the [compute_sensitivity]
#' function with inputs `age`, `obesity`, `css_params`, `fit_params`, and
#' `C_ext`. The results will be returned as a named list and stored in the
#' `sensitivity` field of the input GeoTox object.
#' 
#' Values of `NULL` in the `tp_b_mult` input will use the default value stored
#' in the `GeoTox` object (`x$par$resp$tp_b_mult`). When a `GeoTox` object is
#' created this is initialized at `1.5`, but can be changed via the
#' [calculate_response] function or directly in the object.
#' 
#' @seealso \code{\link{compute_sensitivity}}
#'
#' @return The same GeoTox object with added `sensitivity` field.
#' @export
sensitivity_analysis <- function(x,
                                 tp_b_mult = list(NULL, NULL, NULL,
                                                  1.2, NULL)) {
  
  if (!inherits(x, "GeoTox")) {
    stop("Input 'x' must be a GeoTox object.", call. = FALSE)
  }
  if (length(tp_b_mult) != 5) {
    stop("Input 'tp_b_mult' must have a length of 5.", call. = FALSE)
  }
  
  x$sensitivity <- list(
    age        = compute_sensitivity(x,
                                     vary = "age",
                                     tp_b_mult = tp_b_mult[[1]]),
    obesity    = compute_sensitivity(x,
                                     vary = "obesity",
                                     tp_b_mult = tp_b_mult[[2]]),
    css_params = compute_sensitivity(x,
                                     vary = "css_params",
                                     tp_b_mult = tp_b_mult[[3]]),
    fit_params = compute_sensitivity(x,
                                     vary = "fit_params",
                                     tp_b_mult = tp_b_mult[[4]]),
    C_ext      = compute_sensitivity(x,
                                     vary = "C_ext",
                                     tp_b_mult = tp_b_mult[[5]])
  )
  
  x
}
