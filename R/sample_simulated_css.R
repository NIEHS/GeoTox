#' Sample from pre-simulated steady-state plasma concentrations
#'
#' Sample steady-state plasma concentrations (C\eqn{_{ss}}) for individuals from
#' pre-simulated values stored in the 'simulated_css' table in a GeoTox
#' database.
#'
#' The C\eqn{_{ss}} values are sampled from the 'simulated_css' table, which
#' must already exist in the GeoTox database; it can be created using
#' [set_simulated_css()]. The minimum characteristics that are used to match
#' individuals to sets of C\eqn{_{ss}} values are age and weight category
#' ("Normal" or "Obese"). Additional columns (which must exist in both the
#' 'simulated_css' and 'sample' tables) can be specified using the
#' `css_extra_cols` argument. If `css_exta_cols` is provided it will be added to
#' the GeoTox object parameter list, `GT$par`.
#'
#' In addition to the 'simulated_css' table, both 'sample' and 'concentration'
#' tables must also exist in the GeoTox database. The 'sample' table must
#' contain the characteristics of individuals (age, weight category, and any
#' additional columns specified in `css_extra_cols`). One way to create this
#' 'sample' table is by using [set_sample()]. The 'concentration' table will
#' typically be created using [simulate_exposure()] and will be populated with
#' rows for each individual and substance combination where exposure data is
#' available. The sampled C\eqn{_{ss}} values will be stored in a new "C_ss"
#' column in the 'concentration' table.
#'
#' The `substance_order` argument can be used to specify the order in which
#' substances are evaluated when sampling C\eqn{_{ss}} values. The default is to
#' evaluate substances in the order they appear in the 'substance' table.
#' However, if a different order is needed for some reason (e.g., replication of
#' results from a previous GeoTox implementation), the user can provide a named
#' list where the name is the column in the 'substance' table to use for
#' ordering (e.g., "casn") and the value is a vector of substance identifiers in
#' the desired order. If provided, the `substance_order` will be added to the
#' GeoTox object parameter list, `GT$par`.
#'
#' @param GT GeoTox object.
#' @param css_extra_cols Additional columns to match from the 'simulated_css'
#'   table (default NULL).
#' @param substance_order Named list specifying order of substance evaluation
#'   (default NULL).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [set_simulated_css()], [simulate_population()]
#'
#' @examples
#' # Create required tables
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
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   add_exposure(exposure_df) |>
#'   simulate_exposure() |>
#'   set_simulated_css(css_df)
#'
#' # Sample simulated C_ss values
#' GT <- GT |> sample_simulated_css()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables. sample_simulated_css() generated the 'C_ss' column
#' # of the 'concentration' table.
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
#' # Replace sample and css tables with new data including an extra column
#' # Limit to a single substance for simplicity
#' sample_df <- tibble::tribble(
#'   ~FIPS, ~age, ~weight, ~sign,
#'   10000, 25, "Normal", "+",
#'   10000, 35,  "Obese", "-",
#'   20000, 50, "Normal", "+"
#' )
#' css_df <- tibble::tribble(
#'   ~casn, ~age_lb, ~age_ub, ~weight, ~css, ~sign,
#'   "00-00-1",  0, 49, "Normal",   1, "+",
#'   "00-00-1", 50, 99, "Normal",   2, "+",
#'   "00-00-1",  0, 49,  "Obese",  11, "+",
#'   "00-00-1", 50, 99,  "Obese",  12, "+",
#'   "00-00-1",  0, 49, "Normal",  -1, "-",
#'   "00-00-1", 50, 99, "Normal",  -2, "-",
#'   "00-00-1",  0, 49,  "Obese", -11, "-",
#'   "00-00-1", 50, 99,  "Obese", -12, "-"
#' )
#' GT <- GT |>
#'  set_sample(sample_df, overwrite = TRUE) |>
#'  simulate_exposure() |>
#'  set_simulated_css(css_df, overwrite = TRUE)
#'
#' # Sample simulated C_ss values with extra column
#' # Notice how the extra column name is added to GT$par
#' str(GT$par)
#' GT <- GT |> sample_simulated_css(css_extra_cols = "sign")
#' str(GT$par)
#'
#' # Look at new 'concentration' table. Values will be missing for substance 2
#' # since it is not in the new css_df.
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
sample_simulated_css <- function(
    GT, css_extra_cols = NULL, substance_order = NULL
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbListTables(con)
  if (!"simulated_css" %in% tables) {
    stop("No 'simulated_css' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"concentration" %in% tables) {
    stop("No 'concentration' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"sample" %in% tables) {
    stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
  }

  # Update parameters
  if (!is.null(css_extra_cols)) {
    set_par(con, "css_extra_cols", css_extra_cols)
    GT$par$css_extra_cols <- css_extra_cols
  } else if (!is.null(GT$par$css_extra_cols)) {
    css_extra_cols <- GT$par$css_extra_cols
  } else {
    css_extra_cols <- c()
  }
  if (!is.null(substance_order)) {
    substance_order_col <- GT$par$substance_order_col <- names(substance_order)
    substance_order     <- GT$par$substance_order     <- substance_order[[1]]
    set_par(con, "substance_order_col", substance_order_col)
    set_par(con, "substance_order",     substance_order)
  } else if (!is.null(GT$par$substance_order_col)) {
    substance_order_col <- GT$par$substance_order_col
    substance_order     <- GT$par$substance_order
  } else {
    substance_order_col <- NULL
  }

  if (is.null(substance_order_col)) {
    substance_order <- dplyr::tbl(con, "substance") |>
      dplyr::pull("id")
  } else {
    substance_order <- dplyr::tbl(con, "substance") |>
      dplyr::select(tidyselect::all_of(c("id", substance_order_col))) |>
      dplyr::collect() |>
      dplyr::rename(value = 2) |>
      dplyr::arrange(match(.data$value, substance_order)) |>
      dplyr::pull("id")
  }

  css_tbl    <- dplyr::tbl(con, "simulated_css")
  conc_tbl   <- dplyr::tbl(con, "concentration")
  sample_tbl <- dplyr::tbl(con, "sample")

  if (!"C_ss" %in% colnames(conc_tbl)) {
    DBI::dbExecute(con, "ALTER TABLE concentration ADD COLUMN C_ss DOUBLE")
  }

  # Collect simulated C_ss data
  css_cols <- c(
    "id", "substance_id", "age_lb", "age_ub", "weight", "css", css_extra_cols
  )
  css_df <- css_tbl |>
    dplyr::select(tidyselect::all_of(css_cols)) |>
    dplyr::collect() |>
    tidyr::nest(css = c("id", "css")) |>
    dplyr::mutate(
      css = purrr::map(.data$css, \(df) {
        df |> dplyr::arrange(.data$id) |> dplyr::pull("css")
      }),
      idx = dplyr::row_number(),
      age_ub_p1 = .data$age_ub + 1
    )

  conc_cols <- c("id", "sample_id", "substance_id")
  sample_cols <- c("id", "location_id", "age", "weight", css_extra_cols)

  # Sample C_ss data for each location
  location_id <- dplyr::tbl(con, "location") |>
    dplyr::select("id") |>
    dplyr::arrange(.data$id) |>
    dplyr::pull()
  purrr::walk(location_id, function(location_id) {
    conc_df <- conc_tbl |>
      dplyr::select(tidyselect::all_of(conc_cols)) |>
      dplyr::left_join(
        sample_tbl |> dplyr::select(tidyselect::all_of(sample_cols)),
        by = dplyr::join_by("sample_id" == "id")
      ) |>
      dplyr::filter(.data$location_id == .env$location_id) |>
      dplyr::select(-c("location_id")) |>
      dplyr::collect()
    if (nrow(conc_df) == 0) return()
    update <- dplyr::inner_join(
      conc_df,
      css_df |> dplyr::select(-c("css")),
      by = dplyr::join_by(
        "substance_id",
        "age" >= "age_lb",
        "age" < "age_ub_p1",
        "weight",
        !!!rlang::syms(css_extra_cols)
      )
    ) |>
      dplyr::select("id", "sample_id", "substance_id", "idx")
    if (nrow(update) == 0) {
      stop("No matching rows found in 'simulated_css' for the 'concentration' ",
           "and 'sample' tables. Ensure that the substance_id, age, weight, ",
           "and any extra columns match between the tables.", call. = FALSE)
    }
    update <- update |>
      dplyr::arrange(.data$sample_id) |>
      tidyr::nest(data = "id") |>
      dplyr::arrange(match(.data$substance_id, substance_order), .data$idx) |>
      dplyr::mutate(
        C_ss = purrr::map2(.data$data, .data$idx, \(data, idx) {
          # Note: if input vector, x, to sample() is a single numeric value,
          # then sample() will sample from 1:x. Here, we want n values of x,
          # not n samples from 1:x. See Details of ?sample for more info.
          css <- css_df$css[[idx]]
          n <- nrow(data)
          if (length(css) == 1) return(rep(css, n))
          sample(css, n, replace = TRUE)
        })
      ) |>
      dplyr::select("data", "C_ss") |>
      tidyr::unnest(c("data", "C_ss"))
    update_table(con, "concentration", update, reset_seed = GT$par$reset_seed)
  })
  invisible(GT)
}
