#' Store and retrieve boundary geometries
#'
#' Boundary information is stored as serialized 'BLOB' objects in the 'boundary'
#' table.
#'
#' NOTE: This function requires the `sf` package to be installed.
#'
#' This function takes a named list of sf objects and stores them in the
#' database. If a location boundary (default "county") is provided, then the
#' non-geometry fields will be added to the 'location' table and replaced with a
#' 'location_id' value so they can be linked to data in the 'sample' table.
#'
#' @param GT GeoTox object.
#' @param df_list Named list of data frames containing boundary geometries as sf
#'   objects.
#' @param location Name of element in `df_list` that contains location boundary
#'   information (default "county").
#' @param overwrite Logical indicating whether to overwrite existing 'boundary'
#'   table.
#'
#' @returns For `set_boundary()`, the same GeoTox object, invisibly. For
#'   `get_boundary()`, a data frame with columns 'id' (boundary name) and 'data'
#'   (sf object).
#' @export
#'
#' @examples
#' # Setup sf objects
#' county <- sf::st_sf(
#'   FIPS = c(10000, 20000),
#'   geometry = sf::st_sfc(sf::st_point(1:2), sf::st_point(3:4))
#' )
#' state <- sf::st_sf(
#'   STATE = "XYZ",
#'   geometry = sf::st_sfc(sf::st_point(5:6))
#' )
#' df_list <- list(county = county, state = state)
#'
#' # Add boundary to GeoTox database
#' GT <- GeoTox() |> set_boundary(df_list)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "boundary") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Retrieve boundary from GeoTox database
#' boundary <- get_boundary(GT)
#' boundary
#' boundary |> tibble::deframe()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
set_boundary <- function(GT, df_list, location = "county", overwrite = FALSE) {
  if (!rlang::is_installed("sf")) { # nocov start
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  } # nocov end
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  if (!inherits(df_list, "list") ||
      is.null(names(df_list)) ||
      !all(sapply(df_list, inherits, "sf"))) {
    stop("`df_list` must be a named list of sf objects.", call. = FALSE)
  }
  # Link df_list[[location]] to location table
  if (!is.null(location) && !is.null(df_list[[location]])) {
    # Isolate geometry field
    data <- sf::st_drop_geometry(df_list[[location]])
    geom <- sf::st_geometry(df_list[[location]])
    # Add non-geometry fields to the location table
    add_table(con, "location", data, reset_seed = GT$par$reset_seed)
    # Replace non-geometry fields with location foreign key
    loc <- dplyr::tbl(con, "location") |> dplyr::collect()
    loc_id <- data |>
      dplyr::left_join(loc, by = intersect(names(data), names(loc))) |>
      dplyr::pull("id")
    df_list[[location]] <- sf::st_as_sf(data.frame(location_id = loc_id, geom))
  }
  # Serialize the sf objects to BLOBs for storage in the database
  df <- data.frame(
    id = names(df_list),
    data = blob::new_blob(purrr::map(df_list, \(x) serialize(x, NULL)))
  )
  if (DBI::dbExistsTable(con, "boundary") & !overwrite) {
    stop("GeoTox connection already has a 'boundary' table. ",
         "Set `overwrite = TRUE` to replace it.", call. = FALSE)
  }
  write_table(
    con,
    "boundary",
    df,
    field.types = c(data = "BLOB"),
    overwrite = overwrite,
    reset_seed = GT$par$reset_seed
  )
  invisible(GT)
}

#' @param GT GeoTox object.
#' @export
#' @rdname set_boundary
get_boundary <- function(GT) {
  if (!rlang::is_installed("sf")) { # nocov start
    stop(
      "Package \"sf\" must be installed to use this function.",
      call. = FALSE
    )
  } # nocov end
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, "boundary")) {
    stop("No 'boundary' table found in the GeoTox connection.", call. = FALSE)
  }
  dplyr::tbl(con, "boundary") |>
    dplyr::collect() |>
    dplyr::mutate(data = purrr::map(.data$data, unserialize))
}
