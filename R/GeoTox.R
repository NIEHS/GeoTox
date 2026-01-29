#' GeoTox S3 object
#'
#' Create a GeoTox object and connect to the underlying database.
#'
#' The `dbname` will point to a DuckDB database file. If the file does not
#' already exist, a new database will be created. Additional arguments passed
#' via `...` will be forwarded to [DBI::dbConnect()].
#'
#' The `reset_seed` parameter is necessary for replicating results from the
#' previous GeoTox implementation. Some database functions create temporary
#' tables where a random string is appended to the table name. This advances the
#' `.Random.seed` state and causes functions that use randomness to produce
#' different results between the previous and current implementations. Setting
#' `reset_seed = TRUE` will reset the user's `.Random.seed` to the value it had
#' before certain database operations.
#'
#' Various parameters will be stored in the 'par' table. These parameters will
#' also be loaded into the GeoTox object as a list in `GT$par`. The value for
#' `reset_seed` can only be set on initial GeoTox object creation and will be
#' loaded from the 'par' table for existing databases.
#'
#' @param dbname Database file name. Default is a temporary file.
#' @param reset_seed Logical indicating whether to reset the user's global
#'   `.Random.seed` after certain database operations (default `FALSE`).
#' @param ... Additional arguments passed to [DBI::dbConnect()].
#'
#' @returns For `GeoTox()`, a GeoTox S3 object. For `get_con()`, a database
#'   connection.
#' @export
#'
#' @examples
#' # Create a GeoTox object
#' GT <- GeoTox()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # List database tables
#' DBI::dbListTables(con)
#'
#' # Look at the GT parameters
#' dplyr::tbl(con, "par") |> dplyr::collect()
#' str(GT$par)
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
GeoTox <- function(
    dbname = tempfile(fileext = ".duckdb"), reset_seed = FALSE, ...
) {
  db_info <- list(dbdir = dbname, ...)
  GT <- structure(
    list(db_info = db_info),
    class = "GeoTox"
  )
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  if (!DBI::dbExistsTable(con, "par")) {
    sql <- "CREATE TABLE par (name TEXT, value TEXT, idx INTEGER)"
    DBI::dbExecute(con, sql)
    set_par(con, "reset_seed", reset_seed)
  }
  GT$par <- get_par(con)
  GT
}

#' @param GT GeoTox object.
#' @export
#' @rdname GeoTox
get_con <- function(GT) {
  db_info <- GT$db_info
  db_info$drv <- duckdb::duckdb()
  do.call(DBI::dbConnect, db_info)
}

#' @export
print.GeoTox <- function(x, ...) {
  con <- get_con(x)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbListTables(con)
  get_n <- function(table_name) {
    if (table_name %in% tables) {
      dplyr::tbl(con, table_name) |> dplyr::count() |> dplyr::pull("n")
    } else {
      "NA"
    }
  }
  if ("sample" %in% tables) {
    n_sample <- dplyr::tbl(con, "sample") |>
      dplyr::count(.data$location_id) |>
      dplyr::pull("n") |>
      range()
    if (n_sample[1] == n_sample[2]) {
      n_sample <- n_sample[1]
    } else {
      n_sample <- paste0("[", paste(n_sample, collapse = ", "), "]")
    }
  } else {
    n_sample <- NA
  }
  if ("concentration" %in% tables) {
    conc_cols <- setdiff(
      colnames(dplyr::tbl(con, "concentration")),
      c("id", "sample_id", "substance_id", "route_id")
    )
  } else {
    conc_cols <- "NA"
  }
  if ("risk" %in% tables) {
    risk_cols <- setdiff(
      colnames(dplyr::tbl(con, "risk")),
      c("assay_id", "sample_id")
    )
  } else {
    risk_cols <- "NA"
  }
  sens_tbls <- grep("^risk_sensitivity_", tables, value = TRUE)
  if (length(sens_tbls) > 0) {
    sens_tbls <- sub("^risk_sensitivity_", "", sens_tbls)
  } else {
    sens_tbls <- "NA"
  }

  cat("GeoTox object\n")
  cat("Database info:\n")
  purrr::iwalk(x$db_info, \(value, name) {
    if (!is.atomic(value)) value <- class(value)
    cat("  ", name, ": ", value, "\n", sep = "")
  })
  cat("Reset seed: ", x$par$reset_seed, "\n", sep = "")
  cat("Assays: ", get_n("assay"), "\n", sep = "")
  cat("Substances: ", get_n("substance"), "\n", sep = "")
  cat("Locations: ", get_n("location"), "\n", sep = "")
  cat("Population: ", n_sample, "\n", sep = "")
  cat("Concentrations: ", paste(conc_cols, collapse = ", "), "\n", sep = "")
  cat("Risk: ", paste(risk_cols, collapse = ", "), "\n", sep = "")
  cat("Sensitivity: ", paste(sens_tbls, collapse = ", "), "\n", sep = "")
}
