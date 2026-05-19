#' Simulate population characteristics and exposures
#'
#' Simulate age, weight category, exposure rates, and external exposures. Sample
#' from pre-simulated steady-state plasma concentrations.
#'
#' This is a wrapper around several other functions:
#' * [add_age()] and [simulate_age()] for age simulation.
#' * [add_obesity()] and [simulate_obesity()] for weight category simulation.
#' * [add_exposure()] and [simulate_exposure()] for external exposure
#' concentration simulation (C\eqn{_{ext}}).
#' * [simulate_exposure_rate()] for exposure rate simulation.
#' * [sample_simulated_css()] for sampling steady-state plasma concentrations
#' (C\eqn{_{ss}}).
#' * [set_fixed_css()] to prepare C\eqn{_{ss}} values for sensitivity analysis.
#'
#' The user can provide data frames for age, obesity, and exposure data; if any
#' of these are provided, the corresponding add and simulate functions will be
#' called. The user can also specify whether to simulate exposure rates and
#' sample C\eqn{_{ss}} values using the `simulate_rate` and `sample_css`
#' arguments, respectively. If `simulate_rate` is `TRUE`, exposure rate
#' parameters must have been added using [add_exposure_rate_params()]. If
#' `sample_css` is `TRUE`, a table of simulated C\eqn{_{ss}} values must already
#' exist using [set_simulated_css()].
#'
#' ## Additional arguments:
#' \describe{
#' \item{n}{Number of samples to simulate (default 1000). Used in
#' [simulate_age()], [simulate_obesity()], and [simulate_exposure()]. Ignored
#' if the 'sample' table already exists, in which case the existing sample sizes
#' are used.}
#' \item{location}{Column name for location ID (default "FIPS"). Used in
#' [add_age()], [add_obesity()], and [add_exposure()].}
#' \item{overwrite}{Logical indicating whether to overwrite existing values
#' (default FALSE). Used in [simulate_age()], [simulate_obesity()],
#' [simulate_exposure_rate()], and [simulate_exposure()].}
#' \item{substance}{Column name for substance ID (default "casn"). Used in
#' [add_exposure()].}
#' \item{route}{Column name for exposure route (default "route"). Used in
#' [add_exposure()].}
#' \item{rate_extra_cols}{Additional columns to include in exposure_rate table.
#' Used in [simulate_exposure_rate()].}
#' \item{obes_prev, obes_sd}{Column names for obesity prevalence and standard
#' deviation (default "OBESITY_CrudePrev" and "OBESITY_SD", respectively). Used
#' in [simulate_obesity()].}
#' \item{expos_mean, expos_sd}{Column names for exposure concentration mean and
#' standard deviation (default "mean" and "sd", respectively). Used in
#' [simulate_exposure()].}
#' \item{css_extra_cols}{Additional columns to include in simulated_css table.
#' Used in [sample_simulated_css()].}
#' \item{substance_order}{Named list specifying order of substances. Used in
#' [sample_simulated_css()] and [set_fixed_css()].}
#' }
#'
#' @param GT GeoTox object.
#' @param age Data frame with age data.
#' @param obesity Data frame with obesity data.
#' @param exposure Data frame with exposure data.
#' @param simulate_rate Logical indicating whether to simulate exposure rates.
#'   This requires that exposure rate parameters have been added using
#'   [add_exposure_rate_params()].
#' @param sample_css Logical indicating whether to sample steady-state plasma
#'   concentrations (C\eqn{_{ss}}). This requires that a table of simulated
#'   C\eqn{_{ss}} values has been set using [set_simulated_css()]. In addition,
#'   [set_fixed_css()] will be called after sampling to prepare C\eqn{_{ss}}
#'   values for sensitivity analysis.
#' @param ... Additional arguments passed to wrapped functions (see 'Additional
#'   arguments' section of 'Details').
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [add_age()], [simulate_age()], [add_obesity()],
#'   [simulate_obesity()], [add_exposure()], [simulate_exposure()],
#'   [simulate_exposure_rate()], [sample_simulated_css()], [set_fixed_css()]
#'
#' @examples
#' # Example simulation data
#'
#' age_df <- data.frame(
#'   FIPS = rep(c(10000, 20000), each = 19),
#'   AGEGRP = rep(0:18, times = 2),
#'   TOT_POP = 0
#' )
#' # FIPS 10000, populate age group 40-44
#' age_df$TOT_POP[c(1, 10)] = 100
#' # FIPS 20000, populate age groups 50-59
#' age_df$TOT_POP[c(1, 12, 13) + 19] = c(200, 100, 100)
#'
#' obesity_df <- data.frame(
#'   FIPS = c(10000, 20000),
#'   OBESITY_CrudePrev = c(20, 80),
#'   OBESITY_SD = 5
#' )
#'
#' exposure_df <- tibble::tribble(
#'   ~FIPS, ~casn, ~route, ~mean, ~sd,
#'   10000, "00-00-1", "inhalation", 10, 1,
#'   10000, "00-00-2", "inhalation", 20, 1,
#'   20000, "00-00-1", "inhalation", 30, 1,
#'   20000, "00-00-2", "inhalation", 40, 1
#' )
#'
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
#' # Simulate population
#' GT <- GeoTox() |>
#'   add_exposure_rate_params() |>
#'   set_simulated_css(css_df) |>
#'   simulate_population(
#'     age = age_df,
#'     obesity = obesity_df,
#'     exposure = exposure_df,
#'     n = 3
#'   )
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#'
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Note: the 'age', 'weight', 'params', and 'other' columns of the
#' # 'fixed_css' table contain the C_ss values for sensitivity analysis.
#' # For example, the 'age' column doesn't contain ages, but C_ss values.
#' dplyr::tbl(con, "fixed_css") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
simulate_population <- function(
    GT, age = NULL, obesity = NULL, exposure = NULL,
    simulate_rate = TRUE, sample_css = TRUE, ...
) {
  dots <- list(...)

  n         <- dots$n         %||% 1e3
  location  <- dots$location  %||% "FIPS"
  overwrite <- dots$overwrite %||% FALSE

  # Prevent partial matching issue
  if ("substance" %in% names(dots)) {
    # Still allow for dots$substance = NULL
    substance <- dots$substance %||% "casn"
  } else {
    substance <- "casn"
  }

  # Age
  if (!is.null(age)) {
    GT <- GT |>
      add_age(age, location = location) |>
      simulate_age(n = n, overwrite = overwrite)
  }

  # Exposure rate
  con <- get_con(GT)
  age_col_exists <- DBI::dbExistsTable(con, "sample") &&
    "age" %in% colnames(dplyr::tbl(con, "sample"))
  rate_tbl_exists <- DBI::dbExistsTable(con, "exposure_rate")
  DBI::dbDisconnect(con)
  if (simulate_rate && (age_col_exists & (!rate_tbl_exists | overwrite))) {
    GT <- GT |>
      simulate_exposure_rate(
        rate_extra_cols = dots$rate_extra_cols,
        overwrite       = overwrite
      )
  }

  # Weight
  if (!is.null(obesity)) {
    GT <- GT |>
      add_obesity(obesity, location = location) |>
      simulate_obesity(
        n         = n,
        overwrite = overwrite,
        obes_prev = dots$obes_prev,
        obes_sd   = dots$obes_sd
      )
  }

  # Exposure
  if (!is.null(exposure)) {
    GT <- GT |>
      add_exposure(
        exposure,
        location  = location,
        # This can result in a partial match error if "substance" is not in dots
        # but "substance_order" is.
        # substance = dots$substance %||% "casn",
        substance = substance,
        route     = dots$route %||% "route"
      ) |>
      simulate_exposure(
        n          = n,
        overwrite  = overwrite,
        expos_mean = dots$expos_mean,
        expos_sd   = dots$expos_sd
      )
  }

  # C_ss
  if (sample_css) {
    GT <- GT |>
      sample_simulated_css(
        css_extra_cols  = dots$css_extra_cols,
        substance_order = dots$substance_order
      ) |>
      set_fixed_css()
  }

  invisible(GT)
}
