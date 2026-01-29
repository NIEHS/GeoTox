#' Set sample data
#'
#' Create the 'sample' table in the GeoTox database.
#'
#' The 'sample' table consists of individual characteristics and location
#' information. At a minimum, it must contain columns with information on age,
#' weight category, and location identifier(s).
#'
#' When the `df` input contains information on age or weight category, the
#' column names must be "age" and "weight", respectively. The `location` input
#' (default "FIPS") can be a named vector to specify multiple identifier columns
#' in `df`. For example, `location = c(FIPS = "FIPS", state = "ST")` would
#' indicate that `df` contains both FIPS codes and state identifiers for
#' locations. The location column(s) will be added to the 'location' table and
#' replaced with a corresponding "location_id" column in the 'sample' table. The
#' `state = "ST"` part in the example above would rename the "ST" column in `df`
#' to "state" in the 'location' table.
#'
#' There are several other functions that can be used to create or add to the
#' 'sample' table: [simulate_age()] to generate age data, [simulate_obesity()]
#' to generate weight category data, and [simulate_population()], which is a
#' wrapper function that can generate both age and weight category data along
#' with other fields. `set_sample()` can be used in combination with these
#' functions to first set some known sample data, e.g. age, and then simulate
#' any missing fields, e.g. weight category.
#'
#' If `overwrite = TRUE`, any existing 'concentration' and 'risk' tables will be
#' dropped before creating the 'sample' table. This is because the existing
#' concentration and risk data would no longer be valid for the new sample data.
#' Further downstream tables are not dropped automatically, but can be updated
#' during subsequent simulation and analysis steps.
#'
#' @param GT GeoTox object.
#' @param df Data frame containing sample data.
#' @param location Column name(s) in `df` that contain location identifier(s)
#'   (default "FIPS").
#' @param overwrite Logical indicating whether to overwrite existing 'sample'
#'   table and remove existing 'concentration' and 'risk' tables (default
#'   FALSE).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [simulate_age()], [simulate_obesity()], [simulate_population()]
#'
#' @examples
#' # Example sample data
#' sample_df <- tibble::tribble(
#'   ~FIPS, ~age, ~weight,
#'   10000, 25, "Normal",
#'   10000, 35,  "Obese",
#'   20000, 50, "Normal"
#' )
#'
#' # Set sample data
#' GT <- GeoTox() |> set_sample(sample_df)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
set_sample <- function(GT, df, location = "FIPS", overwrite = FALSE) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys <- list("location_id" = location)
  check_foreign_keys(df, foreign_keys)
  conc_exists <- DBI::dbExistsTable(con, "concentration")
  risk_exists <- DBI::dbExistsTable(con, "risk")
  if (overwrite) {
    if (conc_exists) {
      DBI::dbRemoveTable(con, "concentration")
    }
    if (risk_exists) {
      DBI::dbRemoveTable(con, "risk")
    }
  } else {
    if (conc_exists | risk_exists) {
      stop("The 'concentration' or 'risk' tables already exist. ",
           "Use `overwrite = TRUE` to drop them.",
           call. = FALSE)
    }
  }
  df <- fetch_foreign_keys(con, df, foreign_keys, GT$par$reset_seed)
  overwrite_table(
    con, "sample", df, foreign_keys, reset_seed = GT$par$reset_seed
  )
  invisible(GT)
}
