#' @export
#' @rdname get_risk_values
get_assay_table <- function(GT) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, "assay")) {
    stop("Assay table not found in database.", call. = FALSE)
  }

  dplyr::tbl(con, "assay") |> dplyr::collect()
}
