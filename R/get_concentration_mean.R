#' Get concentration mean values
#'
#' Calculates the concentration mean values for each substance and route at each
#' location based on the data in the 'concentration' and 'sample' tables of the
#' GeoTox database. The output of this function is useful for plotting or
#' further analysis.
#'
#' The `col` parameter specifies which column in the 'concentration' table to
#' use for the mean calculation. The available choices will depend on what data
#' has been stored in the 'concentration' table during the simulation process.
#'
#' @param GT GeoTox object.
#' @param col Column name in the 'concentration' table for which to calculate
#'   the mean, grouped by substance, route, and location.
#'
#' @returns A data frame.
#' @export
#'
#' @examples
#' # Setup required tables
#' exposure_df <- tibble::tribble(
#'   ~FIPS, ~casn, ~route, ~mean, ~sd,
#'   10000, "00-00-1", "inhalation", 10, 1,
#'   10000, "00-00-2", "inhalation", 20, 1,
#'   20000, "00-00-1", "inhalation", 30, 1,
#'   20000, "00-00-2", "inhalation", 40, 1
#' )
#' GT <- GeoTox() |>
#'   add_exposure(exposure_df) |>
#'   simulate_exposure(n = 100)
#'
#' # Calculate mean external concentration by substance and location
#' get_concentration_mean(GT, "C_ext")
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at column names in the 'concentration' table
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
get_concentration_mean <- function(GT, col) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, "concentration")) {
    stop("No 'concentration' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, "sample")) {
    stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
  }

  out <- dplyr::tbl(con, "concentration") |>
    dplyr::select(tidyselect::all_of(
      c("substance_id", "route_id", "sample_id", col)
    )) |>
    # Get location info from sample table
    dplyr::left_join(
      dplyr::tbl(con, "sample"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    duckplyr::as_duckdb_tibble()

  # Get summary stat
  out |>
    dplyr::summarize(
      mean = mean(.data[[col]], na.rm = TRUE),
      .by = c("substance_id", "route_id", "location_id")
    ) |>
    dplyr::collect() |>
    # Fill in missing value with NA
    # tidyr::complete(
    #   tidyr::nesting(substance_id, route_id),
    #   .data$location_id,
    #   fill = list(mean = NA_real_)
    # )
    # Notes:
    # tidyr::nesting(substance_id, route_id) has R CMD check warnings
    # tidyr::nesting(.data$substance_id, .data$route_id) cannot find columns
    # So, complete using all columns, then filter out unwanted combinations
    tidyr::complete(
      .data$substance_id,
      .data$route_id,
      .data$location_id,
      fill = list(mean = NA_real_)
    ) |>
    dplyr::filter(!all(is.na(.data$mean)), .by = c("substance_id", "route_id"))
}
