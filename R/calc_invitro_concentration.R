#' Calculate in vitro concentration
#'
#' Calculates the in vitro concentration (C_invitro) as the product of steady
#' state plasma concentration (C_ss) and internal dose (D_int), and stores the
#' results in the 'C_invitro' column of the 'concentration' table in the GeoTox
#' database.
#'
#' If `sensitivity = TRUE`, 'C_invitro' will be calculated for sensitivity
#' analysis. Typically this shouldn't be used directly by the user, but rather
#' called by [calc_sensitivity()]. In this case, the function will use the
#' 'concentration_sensitivity' table instead of the 'concentration' table.
#'
#' @param GT GeoTox object.
#' @param overwrite Logical indicating whether to overwrite existing 'C_invitro'
#'   values in the 'concentration' table (default FALSE).
#' @param sensitivity Logical indicating whether to simulate in vitro
#'   concentration for sensitivity analysis (default FALSE).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [calc_internal_dose()], [calc_response()]
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
#' css_df <- tibble::tribble(
#'   ~casn, ~age_lb, ~age_ub, ~weight, ~css,
#'   "00-00-1",  0, 49, "Normal", 21,
#'   "00-00-1", 50, 99, "Normal", 22,
#'   "00-00-1",  0, 49,  "Obese", 61,
#'   "00-00-1", 50, 99,  "Obese", 62,
#'   "00-00-2",  0, 49, "Normal", 11,
#'   "00-00-2", 50, 99, "Normal", 12,
#'   "00-00-2",  0, 49,  "Obese", 31,
#'   "00-00-2", 50, 99,  "Obese", 32
#' )
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   set_simulated_css(css_df) |>
#'   add_exposure_rate_params() |>
#'   simulate_population(exposure = exposure_df) |>
#'   calc_internal_dose()
#'
#' # Calculate in vitro concentration
#' GT <- GT |> calc_invitro_concentration()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant tables
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
calc_invitro_concentration <- function(
    GT, overwrite = FALSE, sensitivity = FALSE
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (sensitivity) {
    conc_name <- "concentration_sensitivity"
  } else {
    conc_name <- "concentration"
  }

  if (!DBI::dbExistsTable(con, conc_name)) {
    stop("No '", conc_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }

  conc_tbl <- dplyr::tbl(con, conc_name)

  if (!"C_invitro" %in% colnames(conc_tbl)) {
    sql <- paste("ALTER TABLE", conc_name, "ADD COLUMN C_invitro DOUBLE")
    DBI::dbExecute(con, sql)
  } else {
    if (!overwrite) {
      stop("GeoTox connection already has a 'C_invitro' column in the '",
           conc_name, "' table. Set `overwrite = TRUE` to replace the ",
           "existing values.\n",
           "Warning: Overwriting will not alter downstream values.",
           call. = FALSE)
    }
  }

  update_df <- conc_tbl |>
    dplyr::mutate(C_invitro = .data$C_ss * .data$D_int) |>
    dplyr::select("id", "C_invitro")

  # reset_seed can be left as FALSE because it is only altered if the update_df
  # is copied into the database.
  update_table(con, conc_name, update_df, copy = FALSE)

  invisible(GT)
}
