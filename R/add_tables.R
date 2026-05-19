#' Add age simulation data
#'
#' Create or add to the 'age' table in a GeoTox database.
#'
#' The simulation data must have columns "AGEGRP" and "TOT_POP" and at least one
#' column containing location information (default "FIPS"). Each location must
#' have 19 rows for AGEGRP 0-18, where 1-18 are age groups in increments of 5,
#' e.g. AGEGRP = 5 would be ages 20 to 24, and AGEGRP = 0 is the combination of
#' all age groups. The age data is used by [simulate_age()] to generate age
#' samples for each location.
#'
#' The `location` input can be a named vector to specify multiple identifier
#' columns in `df`. For example, `location = c(FIPS = "FIPS", state = "ST")`
#' would indicate that `df` contains both FIPS codes and state identifiers for
#' locations. The `state = "ST"` part would rename the "ST" column in `df` to
#' "state" in the 'location' table.
#'
#' @param GT GeoTox object.
#' @param df Data frame with age simulation data.
#' @param location Column name(s) in `df` that contain location identifier(s)
#'   (default "FIPS").
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [simulate_age()]
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
#' # Add age simulation data to GeoTox database
#' GT <- GeoTox() |> add_age(age_df)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "age") |> dplyr::filter(TOT_POP > 0) |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
add_age <- function(GT, df, location = "FIPS") {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys <- list("location_id" = location)
  add_table(con, "age", df, foreign_keys, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#=======================================
# Assay
#=======================================
add_assay <- function(GT, df, substance = "casn") {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys <- list("substance_id" = substance)
  add_table(con, "assay", df, foreign_keys, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#' Add exposure simulation data
#'
#' Create or add to the 'exposure' table in a GeoTox database.
#'
#' The simulation data must contain columns for exposure mean and standard
#' deviation (default "mean" and "sd", respectfully), at least one column
#' containing location information (default "FIPS"), at least one column
#' containing substance information (default "casn"), and one column containing
#' route information (default "route"). The exposure data is used by
#' [simulate_exposure()] to generate external exposure concentration samples for
#' each location.
#'
#' The `location` and `substance` inputs can be named vectors to specify
#' multiple identifier columns in `df`. For example, `substance = c(casn =
#' "casn", name = "chnm")` would indicate that `df` contains both CAS numbers
#' and chemical names for substances. The `name = "chnm"` part would rename the
#' "chnm" column in `df` to "name" in the 'substance' table.
#'
#' @param GT GeoTox object.
#' @param df Data frame with exposure simulation data.
#' @param location Column name(s) in `df` that contain location identifier(s)
#'   (default "FIPS").
#' @param substance Column name(s) in `df` that contain substance identifier(s)
#'   (default "casn").
#' @param route Column name in `df` that contains route identifier (default
#'   "route").
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [simulate_exposure()]
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
#' # Add exposure simulation data to GeoTox database
#' GT <- GeoTox() |> add_exposure(exposure_df)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#' dplyr::tbl(con, "exposure") |> dplyr::collect()
#' dplyr::tbl(con, "location") |> dplyr::collect()
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Add another substance with a new field and route
#' exposure_df <- tibble::tribble(
#'   ~FIPS, ~casn, ~chnm, ~route, ~mean, ~sd,
#'   10000, "00-00-3", "chem3", "drinking", 100, 1,
#'   20000, "00-00-3", "chem3", "drinking", 200, 1
#' )
#' GT |> add_exposure(exposure_df, substance = c(casn = "casn", name = "chnm"))
#'
#' # Look at updated tables
#'
#' dplyr::tbl(con, "exposure") |> dplyr::collect()
#'
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#'
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
add_exposure <- function(
    GT, df, location = "FIPS", substance = "casn", route = "route"
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys = list(
    "location_id" = location,
    "substance_id" = substance,
    "route_id" = route
  )
  add_table(con, "exposure", df, foreign_keys, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#' Add exposure rate simulation data
#'
#' Create or add to the 'exposure_rate_params' table in a GeoTox database.
#'
#' There are two routes with built-in default exposure rate parameters:
#' "inhalation" and "drinking". If `params` is not provided (`NULL`), the
#' default parameters for the specified `route` will be used. If `params` is
#' provided, it must contain columns "age_lb", "age_ub", "mean", and "sd". The
#' exposure rate parameters are used by [simulate_exposure_rate()] to generate
#' exposure rate samples for each individual in the population.
#'
#' The build-in parameters come from the following sources:
#' * Inhalation: [EPA Exposure Factors Handbook (2015), Table 6.7](https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf)
#' in m\eqn{^3}/kg-day. The "mean" and "sd" values are the average of male and
#' female values.
#' * Drinking: [EPA Exposure Factors Handbook (2019), Table 3-30](https://www.epa.gov/sites/default/files/2019-02/documents/efh_-_chapter_3_update.pdf)
#' in mL/kg-day.
#'
#' @param GT GeoTox object.
#' @param route Exposure route (default "inhalation").
#' @param params Data frame with exposure rate parameters. If `NULL`, default
#'   parameters for the specified `route` will be used (default `NULL`).
#' @param overwrite Logical indicating whether to overwrite existing exposure
#'   rate parameters for the specified `route` (default `FALSE`).
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [simulate_exposure_rate()]
#'
#' @examples
#' # Add both default params to GeoTox database
#' GT <- GeoTox() |>
#'   add_exposure_rate_params() |>
#'   add_exposure_rate_params(route = "drinking")
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant tables
#' params_tbl <- dplyr::tbl(con, "exposure_rate_params") |> dplyr::collect()
#'
#' params_tbl |> dplyr::filter(route_id == 1)
#'
#' params_tbl |> dplyr::filter(route_id == 2)
#'
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Add custom params with new column (gender), assign to route "custom"
#' params_df <- tibble::tribble(
#'   ~age_lb, ~age_ub, ~gender, ~mean, ~sd,
#'    0, 49, "male",   10, 1,
#'   50, 99, "male",   20, 1,
#'    0, 49, "female", 30, 1,
#'   50, 99, "female", 40, 1
#' )
#' GT |> add_exposure_rate_params(params_df, route = "custom")
#'
#' # Look at updated tables
#' params_tbl <- dplyr::tbl(con, "exposure_rate_params") |> dplyr::collect()
#'
#' params_tbl |> dplyr::filter(route_id == 1)
#'
#' params_tbl |> dplyr::filter(route_id == 2)
#'
#' params_tbl |> dplyr::filter(route_id == 3)
#'
#' dplyr::tbl(con, "route") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
add_exposure_rate_params <- function(
    GT, route = "inhalation", params = NULL, overwrite = FALSE
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  if (is.null(params)) {
    if (route == "inhalation") {
      # https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf
      # Table 6.7 Distribution percentiles of physiological daily inhalation
      # rates per unit body weight (m^3/kg-day) for free living normal weight
      # males and females aged 2 months to 96 years
      params <- tibble::tribble(
        ~age, ~male.mean, ~male.sd, ~female.mean, ~female.sd,
        0, 0.495, 0.08, 0.48, 0.075,
        1, 0.48,  0.06, 0.45, 0.08,
        2, 0.44,  0.04, 0.44, 0.07,
        5, 0.42,  0.05, 0.40, 0.05,
        7, 0.37,  0.06, 0.35, 0.06,
        11, 0.30,  0.05, 0.27, 0.05,
        23, 0.25,  0.04, 0.23, 0.04,
        30, 0.24,  0.03, 0.24, 0.04,
        40, 0.23,  0.04, 0.21, 0.04,
        65, 0.19,  0.03, 0.17, 0.04
      ) |>
        dplyr::rename(age_lb = "age") |>
        dplyr::mutate(
          age_ub = dplyr::lead(.data$age_lb, default = 150),
          mean = rowMeans(dplyr::pick("male.mean", "female.mean")),
          sd = rowMeans(dplyr::pick("male.sd", "female.sd")),
          route = .env$route
        ) |>
        dplyr::select("route", "age_lb", "age_ub", "mean", "sd")
    } else if (route == "drinking") {
      # https://www.epa.gov/sites/default/files/2019-02/documents/efh_-_chapter_3_update.pdf
      # Table 3-30
      # mL/kg-day
      params <- tibble::tribble(
        ~age, ~mean, ~sd,
        0, 43.5, 42.5,
        1, 46.8, 28.1,
        4, 37.9, 21.8,
        7, 26.9, 15.3,
        11, 20.2, 11.6,
        15, 16.4,  9.6,
        20, 18.6, 10.7,
        45, 22,   10.8,
        65, 21.9,  9.9,
        75, 21.6,  9.5
      ) |>
        dplyr::rename(age_lb = "age") |>
        dplyr::mutate(
          age_ub = dplyr::lead(.data$age_lb, default = 150),
          route = route
        ) |>
        dplyr::select("route", "age_lb", "age_ub", "mean", "sd")
    } else {
      stop("Default exposure params do not exist for route '", route, "'.",
           call. = FALSE)
    }
  } else {
    cols <- c("age_lb", "age_ub", "mean", "sd")
    if (!all(cols %in% names(params))) {
      stop("`params` must contain the following columns: ",
           paste(cols, collapse = ", "), ".", call. = FALSE)
    }
    cols <- c("route", cols)
    if ("route" %in% names(params)) {
      stop("`params` should not contain a 'route' column. ",
           "The route is set by the `route` argument.", call. = FALSE)
    }
    params <- params |>
      dplyr::mutate(route = .env$route) |>
      dplyr::select(tidyselect::all_of(c(cols, setdiff(names(params), cols))))
  }
  add_table(
    con, "route", data.frame(route = route), reset_seed = GT$par$reset_seed
  )
  if (DBI::dbExistsTable(con, "exposure_rate_params")) {
    route_id <- dplyr::tbl(con, "route") |>
      dplyr::filter(.data$route == .env$route) |>
      dplyr::pull("id")
    n <- dplyr::tbl(con, "exposure_rate_params") |>
      dplyr::filter(.data$route_id == .env$route_id) |>
      dplyr::count() |>
      dplyr::pull("n")
    if (n > 0) {
      if (!overwrite) {
        stop("Exposure rate parameters for route '", route, "' already exist. ",
             "Set `overwrite = TRUE` to replace them.", call. = FALSE)
      } else {
        sql <- paste0(
          "DELETE FROM exposure_rate_params WHERE route_id = ", route_id
        )
        DBI::dbExecute(con, sql)
      }
    }
  }
  add_table(
    con, "exposure_rate_params", params,
    foreign_keys = list("route_id" = "route"), reset_seed = GT$par$reset_seed
  )
  invisible(GT)
}

#' Add Hill model parameters
#'
#' Create or add to the 'hill_params' table in a GeoTox database.
#'
#' @param GT GeoTox object.
#' @param hill_params List output from [fit_hill()].
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [fit_hill()]
#'
#' @examples
#' # Example Hill model data
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
#' hill_params <- fit_hill(
#'   hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
#' )
#'
#' # Add Hill model parameters to GeoTox database
#' GT <- GeoTox() |> add_hill_params(hill_params)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "hill_params") |> dplyr::collect()
#'
#' dplyr::tbl(con, "assay") |> dplyr::collect()
#'
#' dplyr::tbl(con, "substance") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
add_hill_params <- function(GT, hill_params) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  if (is.null(hill_params$assay) & is.null(hill_params$substance)) {
    foreign_keys <- NULL
  } else {
    foreign_keys <- list()
    if (!is.null(hill_params$assay)) {
      foreign_keys$assay_id <- hill_params$assay
    }
    if (!is.null(hill_params$substance)) {
      foreign_keys$substance_id <- hill_params$substance
    }
  }
  add_table(
    con, "hill_params", hill_params$fit, foreign_keys,
    reset_seed = GT$par$reset_seed
  )
  invisible(GT)
}

#=======================================
# Location
#=======================================
add_location <- function(GT, df) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  add_table(con, "location", df, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#' Add obesity simulation data
#'
#' Create or add to the 'obesity' table in a GeoTox database.
#'
#' The simulation data must contain columns for obesity prevalence and standard
#' deviation (default "OBESITY_CrudePrev" and "OBESITY_SD", respectfully) and at
#' least one column containing location information (default "FIPS"). The
#' obesity data is used by [simulate_obesity()] to generate weight category
#' samples for each location.
#'
#' The `location` input can be a named vector to specify multiple identifier
#' columns in `df`. For example, `location = c(FIPS = "FIPS", state = "ST")`
#' would indicate that `df` contains both FIPS codes and state identifiers for
#' locations. The `state = "ST"` part would rename the "ST" column in `df` to
#' "state" in the 'location' table.
#'
#' @param GT GeoTox object.
#' @param df Data frame with obesity simulation data.
#' @param location Column name(s) in `df` that contain location identifier(s)
#'   (default "FIPS").
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [simulate_obesity()]
#'
#' @examples
#' # Example obesity simulation data
#' obesity_df <- data.frame(
#'   FIPS = c(10000, 20000),
#'   OBESITY_CrudePrev = c(20, 80),
#'   OBESITY_SD = 5
#' )
#'
#' # Add obesity simulation data to GeoTox database
#' GT <- GeoTox() |> add_obesity(obesity_df)
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at created tables
#'
#' dplyr::tbl(con, "obesity") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Add another location with additional information
#' obesity_df <- data.frame(
#'  FIPS = 30000,
#'  ST = "State3",
#'  OBESITY_CrudePrev = 50,
#'  OBESITY_SD = 10
#' )
#' GT |> add_obesity(obesity_df, location = c(FIPS = "FIPS", state = "ST"))
#'
#' # Look at updated tables
#'
#' dplyr::tbl(con, "obesity") |> dplyr::collect()
#'
#' dplyr::tbl(con, "location") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
add_obesity <- function(GT, df, location = "FIPS") {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  foreign_keys <- list("location_id" = location)
  add_table(con, "obesity", df, foreign_keys, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#=======================================
# Route
#=======================================
add_route <- function(GT, df) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  add_table(con, "route", df, reset_seed = GT$par$reset_seed)
  invisible(GT)
}

#=======================================
# Substance
#=======================================
add_substance <- function(GT, df) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))
  add_table(con, "substance", df, reset_seed = GT$par$reset_seed)
  invisible(GT)
}
