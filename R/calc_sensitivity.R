#' Calculate risk sensitivity to a single variable
#'
#' Compute risk by varying one variable while holding others fixed.
#'
#' The sensitivity analysis makes use of the C\eqn{_{ss}} values stored in the
#' 'fixed_css' table of the GeoTox database. These values are determined using
#' the pre-simulated C\eqn{_{ss}} values supplied to [set_simulated_css()] and
#' can be set using [set_fixed_css()] prior to running this function. This step
#' is automatically done when using [simulate_population()] with `sample_css =
#' TRUE`.
#'
#' There are five options for the `vary` argument:
#' \describe{
#' \item{age}{C\eqn{_{ss}} values from the 'age' column of the 'fixed_css' table
#' are used. For other cases, exposure rates are re-simulated using the median
#' age by location in the 'sample' table by calling [simulate_exposure_rate()]
#' with `sensitivity = TRUE`.}
#' \item{weight}{C\eqn{_{ss}} values from the 'weight' column of the 'fixed_css'
#' table are used.}
#' \item{css_params}{C\eqn{_{ss}} values from the 'params' column of the
#' 'fixed_css' table are used.}
#' \item{fit_params}{C\eqn{_{ss}} values from the 'other' column of the
#' 'fixed_css' table are used. For other cases, the standard deviation of
#' dose-response model fit parameters are set to zero by calling [calc_risk()]
#' with `fixed = TRUE`.}
#' \item{C_ext}{C\eqn{_{ss}} values from the 'other' column of the 'fixed_css'
#' table are used. For other cases, external concentrations are re-simulated
#' with standard deviations set to zero by calling [simulate_exposure()] with
#' `sensitivity = TRUE`.}
#' }
#' In all cases above, the resulting risk table is named
#' 'risk_sensitivity_~vary~' (e.g., 'risk_sensitivity_age') in the GeoTox
#' database.
#'
#' Inputs `rate_extra_cols`, `expos_mean`, and `expos_sd` do not need to be
#' specified again if they were already provided in a previous call and are set
#' in the GeoTox parameters (`GT$par`).
#'
#' @param GT GeoTox object.
#' @param vary Variable to vary. One of "age", "weight", "css_params",
#'   "fit_params", or "C_ext".
#' @param overwrite Logical indicating whether to overwrite existing sensitivity
#'   analysis results in the GeoTox database.
#' @param rate_extra_cols Additional columns to match from the
#'   'exposure_rate_params' table (default NULL).
#' @param expos_mean Column name of exposure concentration mean in the
#'   'exposure' table (default "mean").
#' @param expos_sd Column name of exposure concentration standard deviation in
#'   the 'exposure' table (default "sd").
#' @param max_mult Upper bound multiplier for max response (default 1.5).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [set_simulated_css()], [set_fixed_css()], [sensitivity_analysis()]
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
#' # Calculate sensitivity to age
#' GT <- GT |> calc_sensitivity("age")
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant table
#'
#' dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
#'
#' # Compared to baseline risk table
#' dplyr::tbl(con, "risk") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
calc_sensitivity <- function(
    GT,
    vary = c("age", "weight", "css_params", "fit_params", "C_ext"),
    overwrite = FALSE,
    rate_extra_cols = NULL,
    expos_mean = NULL,
    expos_sd = NULL,
    max_mult = 1.5
) {
  rate_name <- "exposure_rate_sensitivity"
  conc_name <- "concentration_sensitivity"
  con <- get_con(GT)
  on.exit({
    # Remove temporary tables and disconnect
    if (DBI::dbExistsTable(con, rate_name)) DBI::dbRemoveTable(con, rate_name)
    if (DBI::dbExistsTable(con, conc_name)) DBI::dbRemoveTable(con, conc_name)
    DBI::dbDisconnect(con)
  })

  vary <- match.arg(vary)

  tables <- DBI::dbListTables(con)
  if (!"exposure_rate" %in% tables) {
    stop("No 'exposure_rate' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"concentration" %in% tables) {
    stop("No 'concentration' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"fixed_css" %in% tables) {
    stop("No 'fixed_css' table found in the GeoTox connection.",
         call. = FALSE)
  }

  risk_name <- paste0("risk_sensitivity_", vary)
  if (DBI::dbExistsTable(con, risk_name)) {
    if (!overwrite) {
      stop("GeoTox connection already has a '", risk_name, "' table. ",
           "Use `overwrite = TRUE` to replace it.", call. = FALSE)
    }
    DBI::dbRemoveTable(con, risk_name)
  }

  #=====================================
  # Exposure rates
  #=====================================
  # Create temporary table exposure_rate_sensitivity
  rate_tbl <- dplyr::tbl(con, "exposure_rate") |>
    dplyr::compute(
      name = rate_name,
      overwrite = TRUE,
      # Set temporary = FALSE since temporary tables are session-specific.
      # Remove on exit of this function.
      temporary = FALSE
    )
  # Simulate exposure_rate using median age
  if (vary != "age") {
    simulate_exposure_rate(
      GT,
      rate_extra_cols = rate_extra_cols,
      sensitivity = TRUE
    )
  }

  #=====================================
  # External concentration (C_ext)
  #=====================================
  # Create temporary table concentration_sensitivity
  conc_tbl <- dplyr::tbl(con, "concentration") |>
    dplyr::select("id", "sample_id", "substance_id", "route_id", "C_ext") |>
    dplyr::compute(
      name = conc_name,
      overwrite = TRUE,
      # Set temporary = FALSE since temporary tables are session-specific.
      # Remove on exit of this function.
      temporary = FALSE
    )
  # Simulate C_ext values for sd = NA
  if (vary != "C_ext") {
    simulate_exposure(
      GT,
      expos_mean = expos_mean,
      expos_sd = expos_sd,
      sensitivity = TRUE
    )
  }

  #=====================================
  # Steady-state plasma concentration (C_ss)
  #=====================================
  sql <- paste("ALTER TABLE", conc_name, "ADD COLUMN C_ss DOUBLE")
  DBI::dbExecute(con, sql)
  fixed_css_col <- switch(
    vary,
    age        = "age",
    weight     = "weight",
    css_params = "params",
    "other"
  )
  select_cols <- c("sample_id", "substance_id", fixed_css_col)
  update_df <- dplyr::tbl(con, conc_name) |>
    dplyr::left_join(
      dplyr::tbl(con, "fixed_css") |>
        dplyr::select(tidyselect::all_of(select_cols)),
      by = c("sample_id", "substance_id")
    ) |>
    dplyr::select(tidyselect::all_of(c("id", fixed_css_col))) |>
    dplyr::rename(C_ss = 2)
  update_table(con, conc_name, update_df, reset_seed = GT$par$reset_seed)

  #=====================================
  # Internal dose (D_int)
  #=====================================
  calc_internal_dose(GT, sensitivity = TRUE)

  #=====================================
  # in vitro concentration (C_invitro)
  #=====================================
  calc_invitro_concentration(GT, sensitivity = TRUE)

  #=====================================
  # Risk
  #=====================================
  calc_risk(
    GT,
    max_mult = max_mult,
    fixed = vary != "fit_params",
    risk_name = risk_name
  )

  invisible(GT)
}
