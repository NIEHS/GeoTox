#' Set pre-simulated steady-state plasma concentrations
#'
#' Create the 'simulated_css' table in a GeoTox database, which contains
#' pre-simulated steady-state plasma concentrations (C\eqn{_{ss}}).
#'
#' The 'simulated_css' table is used by [sample_simulated_css()] to assign
#' C\eqn{_{ss}} values to individuals. The minimum required columns in the `df`
#' data frame are "age_lb", "age_ub", "weight", "css", and at least one column
#' with substance information (default "casn").
#'
#' The values for "age_lb" and "age_ub" should be non-overlapping integers
#' representing age ranges (in years). For example, two subsequent age groups
#' might be `c(0, 4)` and `c(5, 9)`. The "weight" column should contain the
#' weight category and contain values of either "Normal" or "Obese". The "css"
#' column should contain the pre-simulated C\eqn{_{ss}} values.
#'
#' The `substance` input can be a named vector to specify multiple substance
#' identifier columns in `df`. For example, `c(casn = "casn", name = "chnm")`
#' would indicate that `df` contains both CAS numbers and chemical names for
#' substances. The `name = "chnm"` part would rename the "chnm" column in `df`
#' to "name" in the 'substance' table.
#'
#' @param GT GeoTox object.
#' @param df Data frame containing simulated C\eqn{_{ss}} values for groups of
#'   population characteristics.
#' @param substance Column name(s) in `df` that contain substance identifier(s)
#'   (default "casn").
#' @param overwrite Logical indicating whether to overwrite existing
#'   'simulated_css' table (default FALSE).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [sample_simulated_css()]
#'
#' @examples
#' # Example pre-simulated C_ss data
#' # Note: normally the css_df would have many more rows for each combination of
#' # the non-'css' columns to allow for sampling.
#' css_df <- tibble::tribble(
#'   ~casn, ~age_lb, ~age_ub, ~weight, ~css,
#'   "00-00-1",  0, 49, "Normal",  1,
#'   "00-00-1", 50, 99, "Normal",  2,
#'   "00-00-1",  0, 49,  "Obese", 11,
#'   "00-00-1", 50, 99,  "Obese", 12,
#'   "00-00-2",  0, 49, "Normal", 21,
#'   "00-00-2", 50, 99, "Normal", 22,
#'   "00-00-2",  0, 49,  "Obese", 31,
#'   "00-00-2", 50, 99,  "Obese", 32
#' )
#'
#' # Set simulated C_ss values
#' GT <- GeoTox() |> set_simulated_css(css_df)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant tables
#'
#' dplyr::tbl(con, "simulated_css") |> dplyr::collect()
#'
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
set_simulated_css <- function(GT, df, substance = "casn", overwrite = FALSE) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys <- list("substance_id" = substance)
  check_foreign_keys(df, foreign_keys)
  cols <- c("age_lb", "age_ub", "weight", "css")
  if (!all(cols %in% names(df))) {
    stop("`df` must contain the following columns: ",
         paste(cols, collapse = ", "), ".", call. = FALSE)
  }
  if (DBI::dbExistsTable(con, "simulated_css") && !overwrite) {
    stop("Table 'simulated_css' already exists. ",
         "Set `overwrite = TRUE` to replace it.", call. = FALSE)
  }
  df <- fetch_foreign_keys(con, df, foreign_keys, GT$par$reset_seed)
  overwrite_table(
    con, "simulated_css", df, foreign_keys, reset_seed = GT$par$reset_seed
  )
  invisible(GT)
}
