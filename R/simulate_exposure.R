#' Simulate exposure concentrations
#'
#' Simulate external exposure (C_ext) values to be stored in the
#' 'concentration' table of a GeoTox database.
#'
#' An 'external' table containing simulation data must already exist in the
#' GeoTox database, which is added using [add_exposure()].
#'
#' The inputs `expos_mean` and `expos_sd` will be assigned default values of
#' "mean" and "sd", respectively, if not provided and not already set in the
#' GeoTox object's parameters, `GT$par`. If not `NULL`, the provided values will
#' also be saved to `GT$par`.
#'
#' If `sensitivity = TRUE`, exposure concentrations will be simulated for
#' sensitivity analysis. Typically this shouldn't be used directly by the user,
#' but rather called by [calc_sensitivity()]. In this case, the function will
#' use the 'concentration_sensitivity' table instead of the 'concentration'
#' table, and will assume that the 'sample' table already exists.
#'
#' @param GT GeoTox object.
#' @param n Number of individuals to simulate per location (default 1000).
#'   Ignored if 'sample' table already exists.
#' @param overwrite Logical indicating whether to overwrite existing 'C_ext'
#'   values in the 'concentration' table (default FALSE).
#' @param expos_mean Column name of exposure concentration mean in the
#'   'exposure' table (default "mean").
#' @param expos_sd Column name of exposure concentration standard deviation in
#'   the 'exposure' table (default "sd").
#' @param sensitivity Logical indicating whether to simulate exposures for
#'   sensitivity analysis (default FALSE).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [add_exposure()], [simulate_population()]
#'
#' @examples
#' # Example exposure simulation data
#' exposure_df <- tibble::tribble(
#'   ~FIPS, ~casn, ~route, ~mean, ~sd,
#'   10000, "00-00-1", "inhalation", 10, 1,
#'   10000, "00-00-2", "inhalation", 20, 1,
#'   20000, "00-00-1", "inhalation", 30, 1,
#'   20000, "00-00-2", "inhalation", 40, 1
#' )
#'
#' # Simulate C_ext values
#' GT <- GeoTox() |>
#'   add_exposure(exposure_df) |>
#'   simulate_exposure(n = 3)
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
#' # Replace 'exposure' table with different column names
#' names(exposure_df)[4:5] <- c("mu", "sigma")
#' DBI::dbRemoveTable(con, "exposure")
#' GT |> add_exposure(exposure_df)
#'
#' # Overwrite 'C_ext' values in existing 'concentration' table
#' # Must specify new 'exposure' column names
#' # Notice how the column names are added to GT$par
#' str(GT$par)
#' GT <- GT |>
#'   simulate_exposure(expos_mean = "mu", expos_sd = "sigma", overwrite = TRUE)
#' str(GT$par)
#'
#' # Look at updated 'concentration' table
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
simulate_exposure <- function(
    GT, n = 1e3, overwrite = FALSE, expos_mean = NULL, expos_sd = NULL,
    sensitivity = FALSE
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  if (!DBI::dbExistsTable(con, "exposure")) {
    stop("No 'exposure' table found in the GeoTox connection.", call. = FALSE)
  }
  if (sensitivity) {
    conc_name <- "concentration_sensitivity"
    # Conc table should have been created by calc_sensitivity()
    if (!DBI::dbExistsTable(con, conc_name)) {
      stop("No '", conc_name, "' table found in the GeoTox connection.",
           call. = FALSE)
    }
    # Sample table should already exist
    if (!DBI::dbExistsTable(con, "sample")) {
      stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
    }
    conc_tbl <- dplyr::tbl(con, conc_name)
  } else {
    conc_name <- "concentration"
  }

  # Update parameters
  if (!is.null(expos_mean)) {
    set_par(con, "expos_mean", expos_mean)
    GT$par$expos_mean <- expos_mean
  } else if (!is.null(GT$par$expos_mean)) {
    expos_mean <- GT$par$expos_mean
  } else {
    expos_mean <- "mean"
  }
  if (!is.null(expos_sd)) {
    set_par(con, "expos_sd", expos_sd)
    GT$par$expos_sd <- expos_sd
  } else if (!is.null(GT$par$expos_sd)) {
    expos_sd <- GT$par$expos_sd
  } else {
    expos_sd <- "sd"
  }

  # Collect all the exposure simulation input data
  exposure_cols <- c(
    "id", "location_id", "substance_id", "route_id", expos_mean, expos_sd
  )
  exposure_df <- dplyr::tbl(con, "exposure") |>
    dplyr::select(tidyselect::all_of(exposure_cols)) |>
    dplyr::collect()
  if (sensitivity) {
    exposure_df <- exposure_df |> dplyr::mutate(sd = NA_real_)
  }
  exposure_df <- exposure_df |>
    tidyr::nest(.by = "location_id") |>
    dplyr::arrange(.data$location_id)

  if (!sensitivity) {
    if (!DBI::dbExistsTable(con, "sample")) {
      # Create sample table
      sql <- paste0(
        "CREATE TABLE sample AS ",
        "SELECT CAST(id AS INTEGER) AS id ",
        "FROM range(1, ", n * nrow(exposure_df) + 1, ") AS t(id)"
      )
      DBI::dbExecute(con, sql)
      DBI::dbExecute(con, "ALTER TABLE sample ADD COLUMN location_id INTEGER")
      # Add location info
      exposure_df |>
        dplyr::mutate(idx = dplyr::row_number()) |>
        dplyr::select("location_id", "idx") |>
        purrr::pwalk(\(location_id, idx) {
          sample_df <- dplyr::bind_cols(
            id = ((idx - 1) * n + 1):(idx * n),
            location_id = location_id
          )
          update_table(con, "sample", sample_df, reset_seed = GT$par$reset_seed)
        })
    }
    if (!DBI::dbExistsTable(con, conc_name)) {
      # Create concentration table
      conc_tbl <- dplyr::cross_join(
        dplyr::tbl(con, "sample") |>
          dplyr::select("id") |>
          dplyr::rename(sample_id = "id"),
        dplyr::tbl(con, "exposure") |>
          dplyr::distinct(.data$substance_id, .data$route_id)
      ) |>
        dplyr::mutate(id = dplyr::row_number(), .before = 1) |>
        dplyr::compute(
          name = conc_name,
          temporary = FALSE
        )
      sql <- paste("ALTER TABLE", conc_name, "ADD COLUMN C_ext DOUBLE")
      DBI::dbExecute(con, sql)
    } else {
      conc_tbl <- dplyr::tbl(con, conc_name)
      if (!"C_ext" %in% colnames(conc_tbl)) {
        sql <- paste("ALTER TABLE", conc_name, "ADD COLUMN C_ext DOUBLE")
        DBI::dbExecute(con, sql)
      } else if (!overwrite) {
        stop("GeoTox connection already has a '", conc_name, "' table with a ",
             "'C_ext' column. Set `overwrite = TRUE` to replace the existing ",
             "values.\n",
             "Warning: Overwriting will not alter downstream values.",
             call. = FALSE)
      }
    }
  }

  # Loop over locations
  exposure_df |>
    purrr::pwalk(\(location_id, data) {
      sample_df <- dplyr::tbl(con, "sample") |>
        dplyr::filter(.data$location_id == .env$location_id) |>
        dplyr::select("id") |>
        dplyr::arrange(.data$id) |>
        dplyr::collect()
      if (nrow(sample_df) == 0) return()
      update_df <- .simulate_exposure(
        data |> dplyr::arrange(.data$substance_id),
        expos_mean,
        expos_sd,
        n = nrow(sample_df)
      )
      colnames(update_df) <- data$id
      update_df <- update_df |>
        dplyr::as_tibble() |>
        dplyr::mutate(sample_id = sample_df$id) |>
        tidyr::pivot_longer(
          cols = !c("sample_id"),
          names_to = "exposure_id",
          values_to = "C_ext"
        ) |>
        dplyr::mutate(exposure_id = as.integer(.data$exposure_id)) |>
        dplyr::left_join(
          data |> dplyr::select("id", "substance_id", "route_id"),
          by = dplyr::join_by("exposure_id" == "id")
        ) |>
        dplyr::select("sample_id", "substance_id", "route_id", "C_ext")
      update_table(
        con, conc_name, update_df, reset_seed = GT$par$reset_seed,
        by = c("sample_id", "substance_id", "route_id")
      )
    })
  invisible(GT)
}

.simulate_exposure <- function(x, mean, sd, n) {

  mean <- x[[mean]]
  sd <- x[[sd]]

  mapply(
    function(mean, sd) {
      if (mean == 0) {
        rep(0, n)
      } else if (mean > 0 & is.na(sd)) {
        rep(mean, n)
      } else {
        truncnorm::rtruncnorm(
          n, a = 0, b = Inf, mean = mean, sd = sd
        )
      }
    },
    mean = mean,
    sd = sd,
    SIMPLIFY = FALSE
  ) |>
    do.call(what = cbind)
}
