#' Set Hill parameters for a GeoTox object.
#'
#' @param x GeoTox object.
#' @param hill_params output of [fit_hill].
#'
#' @return same GeoTox object with Hill parameters set.
#' @export
#'
#' @examples
#' hill_params <- geo_tox_data$dose_response |>
#'   fit_hill(chem = "casn", assay = "endp") |>
#'   dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)
#'
#' geoTox <- GeoTox() |> 
#'   set_hill_params(hill_params)
set_hill_params <- function(x, hill_params) {
  x$hill_params <- hill_params
  # Clear downstream fields
  if (!is.null(x$resp) | !is.null(x$sensitivity)) {
    warning("Clearing 'resp' and 'sensitivity' fields")
    x$resp <- NULL
    x$sensitivity <- NULL
  }
  x
}
