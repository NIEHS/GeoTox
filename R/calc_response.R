#' Calculate concentrations and risks
#'
#' Calculate internal dose, in vitro concentration, and risk estimates.
#'
#' This is a wrapper around several other functions:
#' * [calc_internal_dose()] to calculate internal dose (D_int).
#' * [calc_invitro_concentration()] to calculate in vitro concentration
#' (C_invitro).
#' * [calc_risk()] to calculate risk estimates.
#'
#' If a `risk_name` argument is provided to `...` and it is not "risk", then
#' sensitivity analysis is assumed and the `sensitivity` argument in the
#' internal dose and in vitro concentration calculations is set to TRUE.
#'
#' @param GT GeoTox object.
#' @param overwrite Logical indicating whether to overwrite existing values
#'   (default FALSE).
#' @param ... Additional arguments passed to [calc_risk()].
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [calc_internal_dose()], [calc_invitro_concentration()],
#'   [calc_risk()]
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
#' hill_df <- tibble::tribble(
#'   ~assay, ~casn, ~logc, ~resp,
#'   "a1", "00-00-1",    0,  10,
#'   "a1", "00-00-1",    1,  20,
#'   "a1", "00-00-1",    2,  80,
#'   "a1", "00-00-1",    3, 100,
#'   "a1", "00-00-2", -0.5,   5,
#'   "a1", "00-00-2",  0.5,  20,
#'   "a1", "00-00-2",  1.5,  55,
#'   "a1", "00-00-2",  2.5,  60
#' )
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   set_simulated_css(css_df) |>
#'   add_exposure_rate_params() |>
#'   add_hill_params(fit_hill(hill_df, assay = "assay", substance = "casn")) |>
#'   simulate_population(exposure = exposure_df)
#'
#' # Calculate concentrations and risk
#' GT <- GT |> calc_response()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant table
#'
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' dplyr::tbl(con, "risk") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
calc_response <- function(GT, overwrite = FALSE, ...) {
  dots <- list(...)
  if ("risk_name" %in% names(dots) && dots$risk_name != "risk") {
    sensitivity <- TRUE
  } else {
    sensitivity <- FALSE
  }
  GT <- GT |>
    calc_internal_dose(
      overwrite = overwrite, sensitivity = sensitivity
    ) |>
    calc_invitro_concentration(
      overwrite = overwrite, sensitivity = sensitivity
    ) |>
    calc_risk(overwrite = overwrite, ...)
  invisible(GT)
}
