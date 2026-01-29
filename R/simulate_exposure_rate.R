#' Simulate exposure rates
#'
#' Simulate exposure rate values for each sample and exposure route based on
#' parameters in the `exposure_rate_params` table.
#'
#' An 'exposure_rate_params' table must already exist in the GeoTox database,
#' which is added using [add_exposure_rate_params()]. There must also be a
#' 'sample' table with an 'age' column, which can be created using
#' [simulate_age()] or [set_sample()]. 'location' and 'route' tables must also
#' exist, but they are created when adding the rate parameters and samples.
#'
#' If `rate_exta_cols` is provided it will be added to the GeoTox object
#' parameter list, `GT$par`. These columns must exist in the
#' 'exposure_rate_params' table and will be used to match between the 'sample'
#' and 'exposure_rate_params' tables when simulating exposure rates.
#'
#' Typically this function will be called with `sensitivity` set to `FALSE`. In
#' this case, the function will create an 'exposure_rate' table with simulated
#' exposure rates for each sample and exposure route.
#'
#' If `sensitivity` is `TRUE`, exposure rates will be simulated for sensitivity
#' analysis. Typically this shouldn't be used directly by the user, but rather
#' called by [calc_sensitivity()]. In this case, the existing 'exposure_rate'
#' table will be copied to the 'exposure_rate_sensitivity' table where the rate
#' values will be overwritten.
#'
#' @param GT GeoTox object.
#' @param rate_extra_cols Additional columns to match from the
#'   'exposure_rate_params' table (default NULL).
#' @param overwrite Logical indicating whether to overwrite existing
#'   'exposure_rate' table (default FALSE).
#' @param sensitivity Logical indicating whether to simulate exposure rates for
#'   sensitivity analysis (default FALSE).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [add_exposure_rate_params()], [simulate_population()]
#'
#' @examples
#' # Setup required tables
#' # Note: 'gender' is ignored when using the default rate params
#' sample_df <- tibble::tribble(
#'   ~FIPS, ~age, ~weight, ~gender,
#'   10000, 25, "Normal",   "male",
#'   10000, 35,  "Obese",   "male",
#'   20000, 50, "Normal", "female"
#' )
#' GT <- GeoTox() |>
#'   add_exposure_rate_params() |>
#'   set_sample(sample_df)
#'
#' # Simulate exposure rates
#' GT |> simulate_exposure_rate()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#'
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Replace exposure rate params with a new table that includes gender
#' params_df <- tibble::tribble(
#'   ~age_lb, ~age_ub, ~gender, ~mean, ~sd,
#'    0, 49,   "male", 10, 1,
#'   50, 99,   "male", 20, 1,
#'    0, 49, "female", 30, 1,
#'   50, 99, "female", 40, 1
#' )
#' DBI::dbRemoveTable(con, "exposure_rate_params")
#' GT |> add_exposure_rate_params(params = params_df)
#'
#' # Overwrite 'rate' values in existing 'exposure_rate' table
#' # Must specify additional column names
#' # Notice how the column names are added to GT$par
#' str(GT$par)
#' GT <- GT |>
#'   simulate_exposure_rate(rate_extra_cols = c("gender"), overwrite = TRUE)
#' str(GT$par)
#'
#' # Look at updated 'exposure_rate' table
#' dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
simulate_exposure_rate <- function(
    GT, rate_extra_cols = NULL, overwrite = FALSE, sensitivity = FALSE
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!sensitivity) {
    # Create exposure_rate table
    rate_name <- "exposure_rate"
    tables <- DBI::dbListTables(con)
    if (!"sample" %in% tables) {
      stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
    }
    if (!"location" %in% tables) {
      stop("No 'location' table found in the GeoTox connection.", call. = FALSE)
    }
    if (!"route" %in% tables) {
      stop("No 'route' table found in the GeoTox connection.", call. = FALSE)
    }
    if (!"exposure_rate_params" %in% tables) {
      stop("No 'exposure_rate_params' table found in the GeoTox connection.",
           call. = FALSE)
    }
    if (rate_name %in% tables & !overwrite) {
      stop("GeoTox connection already has an '", rate_name, "' table. ",
           "Set `overwrite = TRUE` to replace it.\n",
           "Warning: Overwriting will not alter downstream values.",
           call. = FALSE)
    }
    rate_tbl <- dplyr::cross_join(
      dplyr::tbl(con, "sample") |>
        dplyr::select("id") |>
        dplyr::rename(sample_id = "id"),
      dplyr::tbl(con, "route") |>
        dplyr::select("id") |>
        dplyr::rename(route_id = "id")
    ) |>
      dplyr::compute(
        name = rate_name,
        temporary = FALSE,
        overwrite = TRUE
      )
    sql <- paste("ALTER TABLE", rate_name, "ADD COLUMN rate DOUBLE")
    DBI::dbExecute(con, sql)
  } else {
    # Table already created in calc_sensitivity()
    rate_name <- "exposure_rate_sensitivity"
    if (!DBI::dbExistsTable(con, rate_name)) {
      stop("No '", rate_name, "' table found in the GeoTox connection.",
           call. = FALSE)
    }
    rate_tbl <- dplyr::tbl(con, rate_name)
  }

  # Update parameters
  if (!is.null(rate_extra_cols)) {
    set_par(con, "rate_extra_cols", rate_extra_cols)
    GT$par$rate_extra_cols <- rate_extra_cols
  } else if (!is.null(GT$par$rate_extra_cols)) {
    rate_extra_cols <- GT$par$rate_extra_cols
  } else {
    rate_extra_cols <- c()
  }

  # Collect all rate params
  params_cols <- c(
    "route_id", "age_lb", "age_ub", "mean", "sd", rate_extra_cols
  )
  params_df <- dplyr::tbl(con, "exposure_rate_params") |>
    dplyr::select(tidyselect::all_of(params_cols)) |>
    dplyr::collect()

  # Needed columns
  sample_cols <- c("id", "location_id", "age", rate_extra_cols)
  select_cols <- c("sample_id", "route_id", "age", rate_extra_cols)

  # Loop over locations
  location_id <- dplyr::tbl(con, "location") |>
    dplyr::select("id") |>
    dplyr::arrange(.data$id) |>
    dplyr::pull()
  purrr::walk(location_id, function(loc) {
    # Each sample + route should only get mapped to a single param mean + sd, so
    # a "many-to-one" relationship is used. However, this isn't supported on
    # database backends, so collect() needs to be done first.
    rate_df <- rate_tbl |>
      dplyr::left_join(
        dplyr::tbl(con, "sample") |>
          dplyr::select(tidyselect::all_of(sample_cols)),
        by = dplyr::join_by("sample_id" == "id")
      ) |>
      dplyr::filter(.data$location_id == .env$loc) |>
      dplyr::select(tidyselect::all_of(select_cols))
    if (sensitivity) {
      rate_df <- rate_df |>
        dplyr::mutate(age = stats::median(.data$age, na.rm = TRUE))
    }
    rate_df <- rate_df |>
      dplyr::arrange(.data$sample_id, .data$route_id) |>
      dplyr::collect()
    if (nrow(rate_df) == 0) return()
    rate_df <- rate_df |>
      dplyr::left_join(
        params_df,
        by = dplyr::join_by(
          "age" >= "age_lb",
          "age" < "age_ub",
          "route_id",
          !!!rlang::syms(rate_extra_cols)
        ),
        relationship = "many-to-one"
      ) |>
      dplyr::mutate(
        rate = truncnorm::rtruncnorm(1, 0, Inf, .data$mean, .data$sd)
      ) |>
      dplyr::select("sample_id", "route_id", "rate")

    update_table(
      con, rate_name, rate_df, by = c("sample_id", "route_id"),
      reset_seed = GT$par$reset_seed
    )
  })
  invisible(GT)
}
