#' Simulate age values
#'
#' Simulate 'age' values to be stored in the 'sample' table of a GeoTox
#' database.
#'
#' An 'age' table containing simulation data must already exist in the GeoTox
#' database, which is added using [add_age()].
#'
#' @param GT GeoTox object.
#' @param n Number of individuals to simulate per location (default 1000).
#'   Ignored if 'sample' table already exists.
#' @param overwrite Logical indicating whether to overwrite existing 'age'
#'   values in the 'sample' table (default FALSE).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [add_age()], [simulate_population()]
#'
#' @examples
#' # Example age simulation data
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
#' # Simulate age values
#' GT <- GeoTox() |>
#'   add_age(age_df) |>
#'   simulate_age(n = 5)
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
#' # Overwrite existing age values
#' GT <- GT |> simulate_age(overwrite = TRUE)
#'
#' # Look at updated 'sample' table
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
simulate_age <- function(GT, n = 1e3, overwrite = FALSE) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, "age")) {
    stop("No 'age' table found in the GeoTox connection.", call. = FALSE)
  }

  # Collect all the age simulation input data
  age_cols <- c("location_id", "AGEGRP", "TOT_POP")
  age_df <- dplyr::tbl(con, "age") |>
    dplyr::select(tidyselect::all_of(age_cols)) |>
    dplyr::collect() |>
    tidyr::nest(.by = "location_id") |>
    dplyr::arrange(.data$location_id) |>
    dplyr::mutate(n_row = purrr::map_int(.data$data, nrow))
  if (any(age_df$n_row != 19)) {
    stop("The 'age' table must contain 19 rows for each location.",
         call. = FALSE)
  } else {
    age_df <- age_df |> dplyr::select(-c("n_row"))
  }

  # Simulate age data for each location
  if (!DBI::dbExistsTable(con, "sample")) {
    # Create table
    # Initialize IDs so update can be used instead of append for each location.
    # Appending to grow the table is much slower for large datasets.
    # if (inherits(con, "SQLiteConnection")) {
    #   RSQLite::initExtension(con, extension = "series")
    #   sql <- paste0(
    #     "CREATE TABLE sample AS ",
    #     "SELECT value AS id ",
    #     "FROM generate_series(1, ", n * nrow(age_df), ", 1)"
    #   )
    # } else {
    sql <- paste0(
      "CREATE TABLE sample AS ",
      "SELECT CAST(id AS INTEGER) AS id ",
      "FROM range(1, ", n * nrow(age_df) + 1, ") AS t(id)"
    )
    # }
    DBI::dbExecute(con, sql)
    # DBI::dbExecute(con, "ALTER TABLE sample ADD PRIMARY KEY (id)")
    DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN location_id INTEGER")
    DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN age INTEGER")
    # Generate ages by location
    age_df |>
      dplyr::mutate(idx = dplyr::row_number()) |>
      purrr::pwalk(\(location_id, data, idx) {
        update_df <- dplyr::bind_cols(
          id = ((idx - 1) * n + 1):(idx * n),
          location_id = location_id,
          age = .simulate_age(data, n)
        )
        update_table(con, "sample", update_df, reset_seed = GT$par$reset_seed)
      })
  } else {
    sample_tbl <- dplyr::tbl(con, "sample")
    if (!"age" %in% colnames(sample_tbl)) {
      DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN age INTEGER")
    } else if (!overwrite) {
      stop("GeoTox connection already has a 'sample' table with an 'age' ",
           "column. Set `overwrite = TRUE` to replace the existing values.\n",
           "Warning: Overwriting will not alter downstream values.",
           call. = FALSE)
    }
    # Generate ages by location
    age_df |>
      purrr::pwalk(\(location_id, data) {
        update_df <- sample_tbl |>
          dplyr::filter(.data$location_id == .env$location_id) |>
          dplyr::select("id") |>
          dplyr::arrange(.data$id) |>
          dplyr::collect()
        if (nrow(update_df) == 0) return()
        update_df <- update_df |>
          dplyr::mutate(age = .simulate_age(data, dplyr::n()))
        update_table(con, "sample", update_df, reset_seed = GT$par$reset_seed)
      })
  }
  invisible(GT)
}

.simulate_age <- function(x, n) {

  # Ensure order for AGEGRP
  x <- x[order(x$AGEGRP), ]

  # Probability of each age group
  prob <- x$TOT_POP[-1]

  # Assume equal probability within group
  prob <- rep(prob / 5, each = 5)

  # Sample ages
  sample(0:89, size = n, prob = prob, replace = TRUE)
}
