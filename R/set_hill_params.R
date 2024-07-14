#' Set Hill parameters for a GeoTox object.
#'
#' @param x GeoTox object.
#' @param hill_params output of [fit_hill].
#'
#' @return same GeoTox object with Hill parameters set.
#' @export
#'
#' @examples
#' geoTox <- GeoTox() |> 
#'   set_hill_params(fit_hill(split(geo_tox_data$dose_response, ~casn)))
set_hill_params <- function(x, hill_params) {
  x$hill_params <- hill_params
  # Clear downstream fields
  if (!is.null(x$resp)) {
    warning("Clearing 'resp' field")
    x$resp <- NULL
  }
  x
}
