#' @param quantiles Numeric vector of quantiles to calculate.
#' @export
#' @rdname get_risk_values
get_risk_quantiles <- function(
    GT,
    metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
    quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
    table_name = "risk"
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  metric <- match.arg(metric)

  if (!DBI::dbExistsTable(con, table_name)) {
    stop("No '", table_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!DBI::dbExistsTable(con, "sample")) {
    stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
  }

  out <- dplyr::tbl(con, table_name) |>
    dplyr::select(tidyselect::all_of(c("assay_id", "sample_id", metric))) |>
    # Get location info from sample table
    dplyr::left_join(
      dplyr::tbl(con, "sample"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    # Convert to duckplyr data frame so reframe works
    duckplyr::as_duckdb_tibble()

  # Get summary stats
  out |>
      dplyr::reframe(
      quantile = quantiles,
      value = stats::quantile(
        .data[[metric]],
        probs = .data$quantile,
        na.rm = TRUE,
        names = FALSE
      ),
      .by = c("assay_id", "location_id")
    ) |>
    dplyr::collect() |>
    # Fill in missing value with NA
    tidyr::complete(
      .data$assay_id,
      .data$location_id,
      .data$quantile,
      fill = list(value = NA_real_)
    )
}
