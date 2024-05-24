#' Perform sensitivity analysis
#'
#' @param x GeoTox object
#'
#' @return The same object with additional fields added or updated
#' @export
sensitivity_analysis <- function(x) {
  
  x$sensitivity <- list(
    age        = compute_sensitivity(x, "age"),
    obesity    = compute_sensitivity(x, "obesity"),
    css_params = compute_sensitivity(x, "css_params"),
    fit_params = compute_sensitivity(x, "fit_params", tp_b_mult = 1.2),
    C_ext      = compute_sensitivity(x, "C_ext")
  )
  
  x
}