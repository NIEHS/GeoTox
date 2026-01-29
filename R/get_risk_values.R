#' Get risk metric results
#'
#' Several functions used to fetch risk metric results from risk tables in a
#' GeoTox database. The outputs of these functions are useful for plotting or
#' further analysis.
#'
#' Normally an 'assay' table is created when adding the Hill model
#' concentration-response fit parameters with [add_hill_params()]. If no `assay`
#' input is specified in [fit_hill()], then an 'assay' table will not be
#' created. For [get_risk_values()], the `assay` parameter can be used to filter
#' results based on assay details stored in the 'assay' table. This is useful
#' when multiple assays are available in the database. The `assay` input should
#' be a named vector specifying the column name and value to filter by, e.g.
#' `assay = c(endp = "mortality")`. For [get_risk_quantiles()], if there is no
#' assay data in the GeoTox database, then the output "assay_id" column will be
#' filled with NA values. If there is assay data, use [get_assay_table()] to
#' retrieve assay information and link the "assay_id" to assay details.
#'
#' Use the `table_name` parameter to specify which risk table to query. There
#' can be several risk tables in a GeoTox database. The default table is named
#' "risk" and is created by either [calc_risk()] or the wrapper function
#' [calc_response()]. Sensitivity analysis results created by either
#' [calc_sensitivity()] or the wrapper function [sensitivity_analysis()] are
#' stored in other tables with names like "risk_sensitivity_age", etc. Refer to
#' [calc_sensitivity()] to see which tables may be available.
#'
#' [get_risk_sensitivity()] is a wrapper function for [get_risk_values()] around
#' all risk tables created by the original risk computation and subsequent
#' sensitivity analysis. The column names are "baseline" for the original risk
#' table, while the other column names correspond to the parameter varied in the
#' sensitivity analysis.
#'
#' @param GT GeoTox object.
#' @param metric Risk metric to retrieve. One of "GCA.Eff", "IA.Eff",
#'   "GCA.HQ.10", or "IA.HQ.10".
#' @param assay Named vector specifying an assay filter (default NULL).
#' @param table_name Name of the risk table to query (default "risk").
#'
#' @returns A data frame or vector.
#' @export
#' @seealso [calc_risk()], [calc_response()], [calc_sensitivity()],
#'   [sensitivity_analysis()], [fit_hill()], [add_hill_params()]
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
#'   ~assay, ~model, ~casn, ~logc, ~resp,
#'   "a1", "human", "00-00-1",    0,  10,
#'   "a1", "human", "00-00-1",    1,  20,
#'   "a1", "human", "00-00-1",    2,  80,
#'   "a1", "human", "00-00-1",    3, 100,
#'   "a1", "human", "00-00-2", -0.5,   5,
#'   "a1", "human", "00-00-2",  0.5,  20,
#'   "a1", "human", "00-00-2",  1.5,  55,
#'   "a1", "human", "00-00-2",  2.5,  60,
#'   "a2",   "rat", "00-00-1",   -1,   0,
#'   "a2",   "rat", "00-00-1",    0,  10,
#'   "a2",   "rat", "00-00-1",    1,  30,
#'   "a2",   "rat", "00-00-1",    2,  40
#' )
#' set.seed(1234)
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   set_simulated_css(css_df) |>
#'   add_exposure_rate_params() |>
#'   add_hill_params(fit_hill(
#'     hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
#'   )) |>
#'   simulate_population(exposure = exposure_df) |>
#'   calc_response() |>
#'   sensitivity_analysis()
#'
#' # Look at 'assay' table contents
#' get_assay_table(GT)
#'
#' # Get "GCA.HQ.10" values from 'risk' table for the "a1" assay
#' get_risk_values(GT, metric = "GCA.HQ.10", assay = c(name = "a1"))
#'
#' # Get "IA.Eff" values from all risk tables for the "a2" assay
#' get_risk_sensitivity(GT, metric = "IA.Eff", assay = c(name = "a2"))
#'
#' # Get "GCA.Eff" quantiles from 'risk' table
#' get_risk_quantiles(GT, metric = "GCA.Eff", quantiles = c(0.25, 0.5, 0.75))
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at the 'risk' table contents
#' dplyr::tbl(con, "risk") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
get_risk_values <- function(
    GT,
    metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
    assay = NULL,
    table_name = "risk"
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  metric <- match.arg(metric)

  if (!DBI::dbExistsTable(con, table_name)) {
    stop("No '", table_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }

  # Process assay input
  assay_tbl_exists <- DBI::dbExistsTable(con, "assay")
  if (!assay_tbl_exists & !is.null(assay)) {
    warning(
      "Assay '", assay, "' specified but 'assay' table not found in database. ",
      "Ignoring assay parameter.", call. = FALSE
    )
    assay <- NULL
  }
  if (assay_tbl_exists) {
    assay_tbl <- dplyr::tbl(con, "assay")
    assay_col <- names(assay)
    # Convert to char in case input was numeric, e.g. assay = c(id = 1)
    assay <- as.character(assay)
    if (is.null(assay_col) || (length(assay_col) != 1 | length(assay) != 1)) {
      stop(
        "A single assay must be specified with the 'assay' input as a named ",
        "vector, i.e. 'assay = c(column = value)'. Use 'get_assay_table()' ",
        "to see available columns and values.", call. = FALSE
      )
    }
    if (!assay_col %in% colnames(assay_tbl)) {
      stop(
        "Column '", assay_col, "' not found in assay table. Use ",
        "'get_assay_table()' to see available columns.", call. = FALSE
      )
    }
    # Rename "id" for after joining tables
    if (assay_col == "id") assay_col <- "assay_id"
  }

  out <- dplyr::tbl(con, table_name)
  if (assay_tbl_exists) {
    out <- out |>
      dplyr::left_join(
        dplyr::tbl(con, "assay"),
        by = dplyr::join_by("assay_id" == "id")
      ) |>
      dplyr::filter(.data[[assay_col]] == !!assay)
    if (dplyr::count(out) |> dplyr::pull() == 0) {
      stop(
        "No results found for assay '", assay, "'. Use 'get_assay_table()' ",
        "to see available assays.", call. = FALSE
      )
    }
  }
  out |>
    dplyr::arrange(.data$sample_id) |>
    dplyr::select(tidyselect::all_of(metric)) |>
    dplyr::pull()
}
