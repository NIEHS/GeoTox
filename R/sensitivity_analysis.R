#' Perform sensitivity analysis
#'
#' Calculate risk sensitivity to all available `vary` parameters in
#' [calc_sensitivity()].
#'
#' Sensitivity is calculated in the order: age, weight, css_params, fit_params,
#' C_ext. The `max_mult` vector allows specifying different upper bound
#' multipliers for each parameter.
#'
#' @param GT GeoTox object.
#' @param max_mult Vector of length 5 containing upper bound multipliers for max
#'   response (default 1.5).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [calc_sensitivity()]
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
#'   simulate_population(exposure = exposure_df) |>
#'   calc_response()
#'
#' # Perform sensitivity analysis
#' GT <- GT |> sensitivity_analysis()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant table
#'
#' dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
#'
#' dplyr::tbl(con, "risk_sensitivity_weight") |> dplyr::collect()
#'
#' dplyr::tbl(con, "risk_sensitivity_css_params") |> dplyr::collect()
#'
#' dplyr::tbl(con, "risk_sensitivity_fit_params") |> dplyr::collect()
#'
#' dplyr::tbl(con, "risk_sensitivity_C_ext") |> dplyr::collect()
#'
#' # Compared to baseline risk table
#' dplyr::tbl(con, "risk") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
sensitivity_analysis <- function(
    GT, max_mult = c(1.5, 1.5, 1.5, 1.5, 1.5)
) {
  GT |>
    calc_sensitivity("age",        max_mult = max_mult[1]) |>
    calc_sensitivity("weight",     max_mult = max_mult[2]) |>
    calc_sensitivity("css_params", max_mult = max_mult[3]) |>
    calc_sensitivity("fit_params", max_mult = max_mult[4]) |>
    calc_sensitivity("C_ext",      max_mult = max_mult[5])
  invisible(GT)
}
