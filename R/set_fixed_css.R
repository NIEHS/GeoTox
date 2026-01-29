#' Prepare steady-state plasma concentrations for sensitivity analysis
#'
#' Create the 'fixed_css' table in the GeoTox database, which contains values of
#' steady-state plasma concentrations (C\eqn{_{ss}}) for sensitivity analysis.
#'
#' Several tables are required in the GeoTox database before the 'fixed_css'
#' table can be created. Typically, `set_fixed_css()` is called after using
#' [sample_simulated_css()], at which point all required tables will be present.
#'
#' The resulting 'fixed_css' table is used for sensitivity analysis where one
#' parameter is allowed to vary at a time while all other parameters are held
#' constant at fixed values.
#'
#' The `substance_order` argument can be used to specify the order in which
#' substances are evaluated when sampling C\eqn{_{ss}} values. The default is to
#' evaluate substances in the order they appear in the 'substance' table.
#' However, if a different order is needed for some reason (e.g., replication of
#' results from a previous GeoTox implementation), the user can provide a named
#' list where the name is the column in the 'substance' table to use for
#' ordering (e.g., "casn") and the value is a vector of substance identifiers in
#' the desired order. If provided, the `substance_order` will be added to the
#' GeoTox object parameter list, `GT$par`. This is only applicable to the
#' "params" section described below.
#'
#' ## Table 'fixed_css' columns:
#' \describe{
#' \item{age}{Median pre-simulated C\eqn{_{ss}} values are computed by age
#' group for each substance, then the median C\eqn{_{ss}} values are assigned
#' to each individual based on their age group.}
#' \item{weight}{Median pre-simulated C\eqn{_{ss}} values are computed by weight
#' category for each substance, then the median C\eqn{_{ss}} values are assigned
#' to each individual based on their weight category.}
#' \item{params}{Individuals are assigned the median age of their location and
#' pre-simulated C\eqn{_{ss}} values for the "Normal" weight category are
#' sampled.}
#' \item{other}{Median sampled C\eqn{_{ss}} values are computed across all
#' substances for each location after mean-imputation of missing C\eqn{_{ss}}
#' values for each substance and location. The median C\eqn{_{ss}} values are
#' assigned to each individual based on their location.}
#' }
#'
#' @param GT GeoTox object.
#' @param substance_order Named list specifying order of substance evaluation
#'   (default NULL).
#'
#' @returns The updated GeoTox object, invisibly.
#' @export
#' @seealso [sample_simulated_css()], [simulate_population()]
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
#'   set_simulated_css(css_df) |>
#'   sample_simulated_css()
#'
#' # Set fixed C_ss values
#' GT <- GT |> set_fixed_css()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#' # Note: the 'age', 'weight', 'params', and 'other' columns of the
#' # 'fixed_css' table contain the C_ss values for sensitivity analysis.
#' # For example, the 'age' column doesn't contain ages, but C_ss values.
#'
#' dplyr::tbl(con, "fixed_css") |> dplyr::collect()
#'
#' dplyr::tbl(con, "concentration") |> dplyr::collect()
#'
#' dplyr::tbl(con, "sample") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
set_fixed_css <- function(GT, substance_order = NULL) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbListTables(con)
  if (!"sample" %in% tables) {
    stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
  }
  if (!"simulated_css" %in% tables) {
    stop("No 'simulated_css' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"substance" %in% tables) {
    stop("No 'substance' table found in the GeoTox connection.", call. = FALSE)
  }
  if (!"concentration" %in% tables) {
    stop("No 'concentration' table found in the GeoTox connection.",
         call. = FALSE)
  }

  # Update parameters
  if (!is.null(substance_order)) {
    substance_order_col <- GT$par$substance_order_col <- names(substance_order)
    substance_order     <- GT$par$substance_order     <- substance_order[[1]]
    set_par(con, "substance_order_col", substance_order_col)
    set_par(con, "substance_order",     substance_order)
  } else if (!is.null(GT$par$substance_order_col)) {
    substance_order_col <- GT$par$substance_order_col
    substance_order     <- GT$par$substance_order
  }

  if (is.null(substance_order)) {
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

  sample_tbl <- dplyr::tbl(con, "sample")
  css_tbl    <- dplyr::tbl(con, "simulated_css")

  # Initialize "fixed_css" table
  fixed_css_tbl <- dplyr::cross_join(
    sample_tbl |> dplyr::select(sample_id = "id"),
    css_tbl |> dplyr::distinct(.data$substance_id)
  ) |>
    dplyr::mutate(id = as.integer(dplyr::row_number()), .before = 1) |>
    dplyr::compute(
      name = "fixed_css",
      temporary = FALSE,
      overwrite = TRUE
    )
  # sql <- "ALTER TABLE fixed_css ADD PRIMARY KEY (id)"
  # DBI::dbExecute(con, sql)

  #=======================================
  # age
  #=======================================

  sql <- "ALTER TABLE fixed_css ADD COLUMN age DOUBLE"
  DBI::dbExecute(con, sql)

  # Compute median C_ss by substance and age group
  css_stats <- css_tbl |>
    dplyr::summarize(
      median_css = stats::median(.data$css, na.rm = TRUE),
      .by = c("substance_id", "age_lb", "age_ub")
    )

  # Join median C_ss by substance and age group to each sample
  df <- fixed_css_tbl |>
    # Get age from sample table
    dplyr::left_join(
      sample_tbl |> dplyr::select("id", "age"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    # Add median_css value
    dplyr::left_join(
      css_stats,
      by = dplyr::join_by(
        "substance_id" == "substance_id",
        "age" >= "age_lb",
        "age" <= "age_ub"
      )
    ) |>
    # Keep only needed columns
    dplyr::select("id", "median_css") |>
    dplyr::rename(age = "median_css")

  # Add values to fixed_css table
  update_table(
    con, "fixed_css", df, copy = FALSE, reset_seed = GT$par$reset_seed
  )

  #=======================================
  # weight
  #=======================================

  sql <- "ALTER TABLE fixed_css ADD COLUMN weight DOUBLE"
  DBI::dbExecute(con, sql)

  # Compute median C_ss by substance and weight group
  css_stats <- css_tbl |>
    dplyr::summarize(
      median_css = stats::median(.data$css, na.rm = TRUE),
      .by = c("substance_id", "weight")
    )

  # Join median C_ss by substance and weight group to each sample
  df <- fixed_css_tbl |>
    # Get weight from sample table
    dplyr::left_join(
      sample_tbl |> dplyr::select("id", "weight"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    # Add median_css value
    dplyr::left_join(
      css_stats,
      by = dplyr::join_by("substance_id", "weight")
    ) |>
    # Keep only needed columns
    dplyr::select("id", "median_css") |>
    dplyr::rename(weight = "median_css")

  update_table(
    con, "fixed_css", df, copy = FALSE, reset_seed = GT$par$reset_seed
  )

  #=======================================
  # params
  #=======================================

  sql <- "ALTER TABLE fixed_css ADD COLUMN params DOUBLE"
  DBI::dbExecute(con, sql)

  # Sample with replacement can't be done easily in database backends. Try to
  # fetch the minimum data needed for sampling in R.

  # First, get median age by location_id.
  median_age <- sample_tbl |>
    dplyr::summarize(
      value = stats::median(.data$age, na.rm = TRUE),
      .by = "location_id"
    )

  # Then, get all C_ss values for "Normal" weight and any age group that
  # includes the median age.
  css_df <- css_tbl |>
    dplyr::filter(.data$weight == "Normal") |>
    # median_age is non-integer and can lay between age_lb and age_ub (e.g. 40.5).
    # Add a new upper bound column that is +1 for the semi_join below
    dplyr::mutate(age_ub_p1 = .data$age_ub + 1) |>
    dplyr::semi_join(
      median_age,
      by = dplyr::join_by("age_lb" <= "value", "age_ub_p1" > "value")
    ) |>
    dplyr::select("substance_id", "age_lb", "age_ub", "css") |>
    dplyr::collect() |>
    tidyr::nest(css = "css")

  # Collect median_age after using in semi_join above
  median_age <- median_age |> dplyr::collect()

  # Sample C_ss values for each location_id and median age
  purrr::walk2(
    median_age$location_id,
    median_age$value,
    \(loc, age) {

      # Collect data for location
      df <- fixed_css_tbl |>
        # Get location_id from sample table
        dplyr::left_join(
          sample_tbl |> dplyr::select("id", "location_id"),
          by = dplyr::join_by("sample_id" == "id")
        ) |>
        dplyr::filter(.data$location_id == .env$loc) |>
        dplyr::select("id", "sample_id", "substance_id") |>
        dplyr::collect()

      df <- df |>
        # Arrange for replication
        dplyr::arrange(.data$sample_id) |>
        tidyr::nest(.by = "substance_id") |>
        dplyr::arrange(match(.data$substance_id, substance_order)) |>
        # Add C_ss values to sample from
        dplyr::left_join(
          css_df |>
            dplyr::filter(
              .data$age_lb <= .env$age,
              .data$age_ub + 1 > .env$age
            ) |>
            dplyr::select("substance_id", "css"),
          by = dplyr::join_by("substance_id")
        ) |>
        # Sample C_ss with replacement
        dplyr::mutate(
          sampled_css = purrr::map2(.data$data, .data$css, \(x, y) {
            # Note: if input vector, x, to sample() is a single numeric value,
            # then sample() will sample from 1:x. Here, we want n values of x,
            # not n samples from 1:x. See Details of ?sample for more info.
            css <- y$css
            n <- nrow(x)
            if (length(css) == 1) {
              params <- rep(css, n)
            } else {
              params <- sample(css, n, replace = TRUE)
            }
            dplyr::bind_cols(
              id = x$id,
              params = params
            )
          })
        ) |>
        dplyr::select("sampled_css") |>
        tidyr::unnest(cols = c("sampled_css"))

      update_table(con, "fixed_css", df, reset_seed = GT$par$reset_seed)
    }
  )

  #=======================================
  # other
  #=======================================

  sql <- "ALTER TABLE fixed_css ADD COLUMN other DOUBLE"
  DBI::dbExecute(con, sql)

  # Compute median C_ss from samples across all substances for each location
  css_stats <- dplyr::tbl(con, "concentration") |>
    dplyr::select("sample_id", "substance_id", "C_ss") |>
    # Get location_id from sample table
    dplyr::left_join(
      sample_tbl |> dplyr::select("id", "location_id"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    # Mean-impute C_ss for each location + substance
    dplyr::mutate(
      mean_css = mean(.data$C_ss, na.rm = TRUE),
      imputed_css = dplyr::if_else(
        is.na(.data$C_ss), .data$mean_css, .data$C_ss
      ),
      .by = c("location_id", "substance_id")
    ) |>
    # Compute median
    dplyr::summarize(
      median_css = stats::median(.data$imputed_css, na.rm = TRUE),
      .by = "location_id"
    )

  # Join median C_ss by location to each sample
  df <- fixed_css_tbl |>
    # Get location_id from sample table
    dplyr::left_join(
      sample_tbl |> dplyr::select("id", "location_id"),
      by = dplyr::join_by("sample_id" == "id")
    ) |>
    # Add median_css value
    dplyr::left_join(
      css_stats,
      by = dplyr::join_by("location_id" == "location_id")
    ) |>
    # Keep only needed columns
    dplyr::select("id", "median_css") |>
    dplyr::rename(other = "median_css")

  update_table(
    con, "fixed_css", df, copy = FALSE, reset_seed = GT$par$reset_seed
  )

  invisible(GT)
}
