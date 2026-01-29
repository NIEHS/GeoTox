#' Simulate obesity values
#'
#' Simulate 'weight' category values to be stored in the 'sample' table of a
#' GeoTox database.
#'
#' An 'obesity' table containing simulation data must already exist in the
#' GeoTox database, which is added using [add_obesity()].
#'
#' The inputs `obes_prev` and `obes_sd` will be assigned default values of
#' "OBESITY_CrudePrev" and "OBESITY_SD", respectively, if not provided and not
#' already set in the GeoTox object's parameters, `GT$par`. If not `NULL`, the
#' provided values will also be saved to `GT$par`.
#'
#' @param GT GeoTox object.
#' @param n Number of individuals to simulate per location (default 1000).
#'   Ignored if 'sample' table already exists.
#' @param overwrite Logical indicating whether to overwrite existing 'weight'
#'   values in the 'sample' table (default FALSE).
#' @param obes_prev Column name of obesity prevalence in the 'obesity' table
#'   (default "OBESITY_CrudePrev").
#' @param obes_sd Column name of obesity standard deviation in the 'obesity'
#'   table (default "OBESITY_SD").
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [add_obesity()], [simulate_population()]
#'
#' @examples
#' # Example obesity simulation data
#' obesity_df <- data.frame(
#'   FIPS = c(10000, 20000),
#'   OBESITY_CrudePrev = c(20, 80),
#'   OBESITY_SD = 5
#' )
#'
#' # Simulate weight category values
#' GT <- GeoTox() |>
#'   add_obesity(obesity_df) |>
#'   simulate_obesity(n = 5)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Replace 'obesity' table with different column names
#' names(obesity_df)[2:3] <- c("prev", "sd")
#' DBI::dbRemoveTable(con, "obesity")
#' GT |> add_obesity(obesity_df)
#'
#' # Overwrite 'weight' category values in existing 'sample' table
#' # Must specify new 'obesity' column names
#' # Notice how the column names are added to GT$par
#' str(GT$par)
#' GT <- GT |>
#'   simulate_obesity(obes_prev = "prev", obes_sd = "sd", overwrite = TRUE)
#' str(GT$par)
#'
#' # Look at updated 'sample' table
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
simulate_obesity <- function(
    GT, n = 1e3, overwrite = FALSE, obes_prev = NULL, obes_sd = NULL
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, "obesity")) {
    stop("No 'obesity' table found in the GeoTox connection.", call. = FALSE)
  }

  # Update parameters
  if (!is.null(obes_prev)) {
    set_par(con, "obes_prev", obes_prev)
    GT$par$obes_prev <- obes_prev
  } else if (!is.null(GT$par$obes_prev)) {
    obes_prev <- GT$par$obes_prev
  } else {
    obes_prev <- "OBESITY_CrudePrev"
  }
  if (!is.null(obes_sd)) {
    set_par(con, "obes_sd", obes_sd)
    GT$par$obes_sd <- obes_sd
  } else if (!is.null(GT$par$obes_sd)) {
    obes_sd <- GT$par$obes_sd
  } else {
    obes_sd <- "OBESITY_SD"
  }

  # Collect all the obesity simulation input data
  obesity_cols <- c("location_id", obes_prev, obes_sd)
  obesity_df <- dplyr::tbl(con, "obesity") |>
    dplyr::select(tidyselect::all_of(obesity_cols)) |>
    dplyr::collect() |>
    dplyr::arrange(.data$location_id) |>
    dplyr::rename(tidyselect::all_of(c("prev" = obes_prev, "sd" = obes_sd)))
  n_dup <- obesity_df |> dplyr::filter(duplicated(.data$location_id)) |> nrow()
  if (n_dup > 0) {
    stop("The 'obesity' table must not contain duplicated location values.",
         call. = FALSE)
  }

  # Simulate weight data for each location
  if (!DBI::dbExistsTable(con, "sample")) {
    # Create table
    # Initialize IDs so update can be used instead of append for each location.
    # Appending to grow the table is much slower for large datasets.
    # if (inherits(con, "SQLiteConnection")) {
    #   RSQLite::initExtension(con, extension = "series")
    #   sql <- paste0(
    #     "CREATE TABLE sample AS ",
    #     "SELECT value AS id ",
    #     "FROM generate_series(1, ", n * nrow(obesity_df), ", 1)"
    #   )
    # } else {
    sql <- paste0(
      "CREATE TABLE sample AS ",
      "SELECT CAST(id AS INTEGER) AS id ",
      "FROM range(1, ", n * nrow(obesity_df) + 1, ") AS t(id)"
    )
    # }
    DBI::dbExecute(con, sql)
    # DBI::dbExecute(con, "ALTER TABLE sample ADD PRIMARY KEY (id)")
    DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN location_id INTEGER")
    DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN weight TEXT")
    # Generate weights by location
    obesity_df |>
      dplyr::mutate(idx = dplyr::row_number()) |>
      purrr::pwalk(\(location_id, prev, sd, idx) {
        sample_df <- dplyr::bind_cols(
          id = ((idx - 1) * n + 1):(idx * n),
          location_id = location_id,
          weight = .simulate_obesity(n, prev, sd)
        )
        update_table(con, "sample", sample_df, reset_seed = GT$par$reset_seed)
      })
  } else {
    sample_tbl <- dplyr::tbl(con, "sample")
    if (!"weight" %in% colnames(sample_tbl)) {
      DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN weight TEXT")
    } else if (!overwrite) {
      stop("GeoTox connection already has a 'sample' table with a 'weight' ",
           "column. Set `overwrite = TRUE` to replace the existing values.\n",
           "Warning: Overwriting will not alter downstream values.",
           call. = FALSE)
    }
    # Generate weights by location
    obesity_df |>
      dplyr::select("location_id", "prev", "sd") |>
      purrr::pwalk(\(location_id, prev, sd) {
        sample_df <- sample_tbl |>
          dplyr::filter(.data$location_id == .env$location_id) |>
          dplyr::select("id") |>
          dplyr::arrange(.data$id) |>
          dplyr::collect()
        if (nrow(sample_df) == 0) return()
        weight <- .simulate_obesity(nrow(sample_df), prev, sd)
        sample_df <- dplyr::bind_cols(sample_df, weight = weight)
        update_table(con, "sample", sample_df, reset_seed = GT$par$reset_seed)
      })
  }
  invisible(GT)
}

.simulate_obesity <- function(n, prev, sd) {
  if (is.na(prev) | is.na(sd)) return(rep(NA_character_, n))
  p <- stats::rnorm(n, prev, sd) / 100
  p[p < 0] <- 0
  p[p > 1] <- 1
  weight <- stats::rbinom(n, 1, p)
  dplyr::if_else(weight == 1, "Obese", "Normal")
}
