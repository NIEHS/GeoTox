#' Set GeoTox boundaries
#'
#' @param x GeoTox object.
#' @param region "sf" data.frame mapping features to a "geometry" column. Used
#' when coloring map regions.
#' @param group "sf" data.frame containing a "geometry" column. Used to draw
#' outlines around groups of regions.
#'
#' @return GeoTox object with boundaries set.
#' 
#' @examples
#' x <- GeoTox() |> 
#'   set_boundaries(region = geo_tox_data$boundaries$county,
#'                  group  = geo_tox_data$boundaries$state)
#' @export
set_boundaries <- function(x, region = NULL, group = NULL) {
  
  if (!is.null(region) && (!inherits(region, "sf") |
                           !("geometry" %in% names(region)))) {
    stop("region must be an 'sf' data.frame with a 'geometry' column.")
  }
  if (!is.null(group) && (!inherits(group, "sf") |
                          !("geometry" %in% names(group)))) {
    stop("group must be an 'sf' data.frame with a 'geometry' column.")
  }

  x$boundaries <- list(
    region = region,
    group = group
  )
  x
}