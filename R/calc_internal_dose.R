#' Calculate internal dose
#'
#' Calculates internal dose (D_int) as the product of external concentration
#' ('C_ext' in the 'concentration' table) and exposure rate ('rate' in the
#' 'exposure_rate' table), and stores the results in the 'D_int' column of the
#' 'concentration' table in the GeoTox database.
#'
#' If `sensitivity = TRUE`, 'D_int' will be calculated for sensitivity
#' analysis. Typically this shouldn't be used directly by the user, but rather
#' called by [calc_sensitivity()]. In this case, the function will use the
#' 'concentration_sensitivity' and 'exposure_rate_sensitivity' tables instead of
#' the 'concentration' and 'exposure_rate' tables.
#'
#' @param GT GeoTox object.
#' @param overwrite Logical indicating whether to overwrite existing 'D_int'
#'   values in the 'concentration' table (default FALSE).
#' @param sensitivity Logical indicating whether to simulate internal dose for
#'   sensitivity analysis (default FALSE).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [calc_response()]
#'
#' @examples
#' # Setup required tables
#' sample_df <- tibble::tribble(
#'   ~FIPS, ~age, ~weight,
#'   10000, 25, "Normal",
#'   10000, 35,  "Obese",
#'   20000, 50, "Normal"
#' )
#' exposure_df <- tibble::tribble(
#'   ~FIPS, ~casn, ~route, ~mean, ~sd,
#'   10000, "00-00-1", "inhalation", 10, 1,
#'   10000, "00-00-2", "inhalation", 20, 1,
#'   20000, "00-00-1", "inhalation", 30, 1,
#'   20000, "00-00-2", "inhalation", 40, 1
#' )
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   add_exposure_rate_params() |>
#'   simulate_population(exposure = exposure_df, sample_css = FALSE)
#'
#' # Calculate internal dose
#' GT <- GT |> calc_internal_dose()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant tables
#'
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
calc_internal_dose <- function(
    GT, overwrite = FALSE, sensitivity = FALSE
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (sensitivity) {
    conc_name <- "concentration_sensitivity"
    rate_name <- "exposure_rate_sensitivity"
  } else {
    conc_name <- "concentration"
    rate_name <- "exposure_rate"
  }

  if (!DBI::dbExistsTable(con, conc_name)) {
    stop("No '", conc_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, rate_name)) {
    stop("No '", rate_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }

  conc_tbl <- dplyr::tbl(con, conc_name)
  rate_tbl <- dplyr::tbl(con, rate_name)

  if (!"D_int" %in% colnames(conc_tbl)) {
    sql <- paste("ALTER TABLE", conc_name, "ADD COLUMN D_int DOUBLE")
    DBI::dbExecute(con, sql)
  } else if (!overwrite) {
    stop("GeoTox connection already has a 'D_int' column in the '", conc_name,
         "' table. Set `overwrite = TRUE` to replace the existing values.\n",
         "Warning: Overwriting will not alter downstream values.",
         call. = FALSE)
  }

  update_df <- conc_tbl |>
    dplyr::left_join(
      rate_tbl,
      by = dplyr::join_by("sample_id", "route_id")
    ) |>
    dplyr::mutate(D_int = .data$C_ext * .data$rate) |>
    dplyr::select("id", "D_int")

  # reset_seed can be left as FALSE because it is only altered if the update_df
  # is copied into the database.
  update_table(con, conc_name, update_df, copy = FALSE)

  invisible(GT)
}
