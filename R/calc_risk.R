#' Calculate risk scores
#'
#' Calculate generalized concentration addition (GCA) and independent action
#' (IA) risk scores and hazard quotients (HQ) based on in vitro concentration
#' data and Hill parameters.
#'
#' This function requires that the 'concentration', 'sample', and 'hill_params'
#' tables are present in the GeoTox object. Typically these tables will have
#' been created by prior calls to [calc_invitro_concentration()] and
#' [add_hill_params()].
#'
#' The risk scores are calculated for each sample and assay combination and
#' stored in the 'risk' table in the GeoTox object, unless a different
#' `risk_name` is provided. Supplying a different `risk_name` shouldn't be done
#' directly by the user, but rather by calling [calc_sensitivity()].
#'
#' @param GT GeoTox object.
#' @param max_mult Upper bound multiplier for max response (default 1.5).
#' @param fixed Logical indicating whether to set standard deviation parameters
#'   of Hill fit to zero (default FALSE). Used in sensitivity analysis.
#' @param overwrite Logical indicating whether to overwrite existing risk table
#'   (default FALSE).
#' @param risk_name Table name to store risk results (default "risk"). Values
#'   other than "risk" are used in sensitivity analysis.
#'
#' @returns The same GeoTox object, invisibly.
#' @export
#' @seealso [add_hill_params()], [calc_invitro_concentration()],
#'   [calc_response()]
#'
#' @examples
#' # Setup required tables
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
#' css_df <- tibble::tribble(
#'   ~casn, ~age_lb, ~age_ub, ~weight, ~css,
#'   "00-00-1",  0, 49, "Normal", 21,
#'   "00-00-1", 50, 99, "Normal", 22,
#'   "00-00-1",  0, 49,  "Obese", 61,
#'   "00-00-1", 50, 99,  "Obese", 62,
#'   "00-00-2",  0, 49, "Normal", 11,
#'   "00-00-2", 50, 99, "Normal", 12,
#'   "00-00-2",  0, 49,  "Obese", 31,
#'   "00-00-2", 50, 99,  "Obese", 32
#' )
#' hill_df <- tibble::tribble(
#'   ~assay, ~casn, ~logc, ~resp,
#'   "a1", "00-00-1",    0,  10,
#'   "a1", "00-00-1",    1,  20,
#'   "a1", "00-00-1",    2,  80,
#'   "a1", "00-00-1",    3, 100,
#'   "a1", "00-00-2", -0.5,   5,
#'   "a1", "00-00-2",  0.5,  20,
#'   "a1", "00-00-2",  1.5,  55,
#'   "a1", "00-00-2",  2.5,  60
#' )
#' GT <- GeoTox() |>
#'   set_sample(sample_df) |>
#'   set_simulated_css(css_df) |>
#'   add_exposure_rate_params() |>
#'   add_hill_params(fit_hill(hill_df, assay = "assay", substance = "casn")) |>
#'   simulate_population(exposure = exposure_df) |>
#'   calc_internal_dose() |>
#'   calc_invitro_concentration()
#'
#' # Calculate risk
#' GT <- GT |> calc_risk()
#'
#' # Open a connection to GeoTox database
#' con <- get_con(GT)
#'
#' # Look at relevant table
#' dplyr::tbl(con, "risk") |> dplyr::collect()
#'
#' # Clean up example
#' DBI::dbDisconnect(con)
#' file.remove(GT$db_info$dbdir)
calc_risk <- function(
    GT, max_mult = 1.5, fixed = FALSE, overwrite = FALSE, risk_name = "risk"
) {
  con <- get_con(GT)
  on.exit(DBI::dbDisconnect(con))

  tables <- DBI::dbListTables(con)

  if (risk_name == "risk") {
    conc_name <- "concentration"
  } else {
    conc_name <- "concentration_sensitivity"
  }
  if (!conc_name %in% tables) {
    stop("No '", conc_name, "' table found in the GeoTox connection.",
         call. = FALSE)
  }
  if (!"sample" %in% tables) {
    stop("No 'sample' table found in the GeoTox connection.", call. = FALSE)
  }
  if (!"hill_params" %in% tables) {
    stop("No 'hill_params' table found in the GeoTox connection.",
         call. = FALSE)
  }

  conc_tbl   <- dplyr::tbl(con, conc_name)
  sample_tbl <- dplyr::tbl(con, "sample")
  hill_tbl   <- dplyr::tbl(con, "hill_params")

  # Check for required columns
  conc_cols <- c("sample_id", "substance_id", "C_invitro")
  sample_cols <- c("id", "location_id")
  hill_cols <- c(
    "substance_id", "tp", "tp.sd", "logAC50", "logAC50.sd", "logc_min",
    "logc_max", "resp_max"
  )
  if ("assay_id" %in% colnames(hill_tbl)) {
    hill_cols <- c("assay_id", hill_cols)
    assay_exists <- TRUE
    update_by <- c("assay_id", "sample_id")
  } else {
    assay_exists <- FALSE
    update_by <- c("sample_id")
  }
  conc_tbl <- conc_tbl |>
    dplyr::select(tidyselect::all_of(conc_cols))
  sample_tbl <- sample_tbl |>
    dplyr::select(tidyselect::all_of(sample_cols))
  hill_df <- hill_tbl |>
    dplyr::select(tidyselect::all_of(hill_cols)) |>
    dplyr::collect()
  if (assay_exists) {
    hill_df <- hill_df |>
      tidyr::nest(.by = "assay_id")
  } else {
    hill_df <- hill_df |>
      tidyr::nest() |>
      dplyr::mutate(assay_id = NA_integer_)
  }

  # If all needed input data are available, check for existing risk table
  if (risk_name %in% tables) {
    if (!overwrite) {
      stop("GeoTox connection already has a '", risk_name, "' table. ",
           "Use `overwrite = TRUE` to replace it.", call. = FALSE)
    }
    DBI::dbRemoveTable(con, risk_name)
  }

  # Create risk table
  if (GT$par$reset_seed) {
    seed <- .Random.seed
    tbls <- DBI::dbListTables(con)
  }
  risk_tbl <- dplyr::cross_join(
    sample_tbl |>
      dplyr::select("id") |>
      dplyr::rename(sample_id = "id"),
    hill_df |> dplyr::select("assay_id"),
    copy = TRUE
  ) |>
    dplyr::select("assay_id", "sample_id") |>
    dplyr::compute(
      name = risk_name,
      temporary = FALSE
    )
  if (GT$par$reset_seed) {
    rm_tbl <- setdiff(DBI::dbListTables(con), c(tbls, risk_name))
    if (length(rm_tbl) == 1) {
      DBI::dbRemoveTable(con, rm_tbl)
    }
    .Random.seed <<- seed
  }
  sql <- paste("ALTER TABLE", risk_name, "ADD COLUMN \"GCA.Eff\" DOUBLE")
  DBI::dbExecute(con, sql)
  sql <- paste("ALTER TABLE", risk_name, "ADD COLUMN \"IA.Eff\" DOUBLE")
  DBI::dbExecute(con, sql)
  sql <- paste("ALTER TABLE", risk_name, "ADD COLUMN \"GCA.HQ.10\" DOUBLE")
  DBI::dbExecute(con, sql)
  sql <- paste("ALTER TABLE", risk_name, "ADD COLUMN \"IA.HQ.10\" DOUBLE")
  DBI::dbExecute(con, sql)

  # Compute risk

  # Loop over location
  location_id <- dplyr::tbl(con, "location") |>
    dplyr::select("id") |>
    dplyr::arrange(.data$id) |>
    dplyr::pull()
  purrr::walk(location_id, \(location_id) {

    # Collect concentration data for all samples in the location
    conc_df <- conc_tbl |>
      dplyr::inner_join(
        sample_tbl |>
          dplyr::filter(location_id == .env$location_id) |>
          dplyr::select("id"),
        by = dplyr::join_by("sample_id" == "id")
      ) |>
      dplyr::filter(.data$C_invitro > 0) |>
      dplyr::collect()

    if (nrow(conc_df) == 0) return(NULL)

    # Loop over assays
    update_df <- purrr::map2(
      hill_df$assay_id,
      hill_df$data,
      \(assay_id, hill_params) {
        risk <- .calc_risk(conc_df, hill_params, max_mult, fixed)
        if (is.null(risk)) return(NULL)
        dplyr::bind_cols(assay_id = assay_id, risk)
      }
    ) |>
      dplyr::bind_rows()

    update_table(
      con, risk_name, update_df, reset_seed = GT$par$reset_seed,
      by = update_by
    )
  })

  invisible(GT)
}

.calc_risk <- function(df, hill_params, max_mult, fixed) {

  interval <- c(-50,50)

  sample_id <- df |>
    dplyr::distinct(.data$sample_id) |>
    dplyr::pull() |>
    sort()

  tp <- lapply(sample_id, \(x) {
    out <- truncnorm::rtruncnorm(
      1,
      a    = 0,
      b    = hill_params$resp_max * max_mult,
      mean = hill_params$tp,
      sd   = if (fixed) 0 else hill_params$tp.sd
    )
    # Replace NAs with 0 (can occur when tp and tp.sd are 0)
    out[is.na(out)] <- 0
    out
  }) |>
    unlist()

  AC50 <- lapply(sample_id, \(x) {
    logAC50 <- truncnorm::rtruncnorm(
      1,
      a    = hill_params$logc_min - 2.0001,
      b    = hill_params$logc_max + 0.5001,
      mean = hill_params$logAC50,
      sd   = if (fixed) 0 else hill_params$logAC50.sd
    )
    10^logAC50
  }) |>
    unlist()

  hill_params <- dplyr::bind_cols(
    sample_id = rep(sample_id, each = nrow(hill_params)),
    substance_id = rep(hill_params$substance_id, times = length(sample_id)),
    tp = tp,
    AC50 = AC50
  )

  df <- df |>
    dplyr::inner_join(
      hill_params,
      by = c("sample_id", "substance_id")
    ) |>
    # Order for replication
    dplyr::arrange(
      .data$sample_id,
      match(.data$substance_id, hill_params$substance_id)
    )

  if (nrow(df) == 0) return(NULL)

  df <- df |>
    tidyr::nest(.by = sample_id) |>
    dplyr::mutate(
      data = purrr::map(.data$data, \(x) {

        mixture.result <- stats::optimize(
          obj_GCA,
          interval = interval,
          conc = x$C_invitro,
          max = x$tp,
          AC50 = x$AC50
        )
        GCA.eff <- exp(mixture.result$minimum)

        Emax_resp <- stats::optimize(
          obj_GCA,
          interval = interval,
          conc = x$C_invitro * 10^14,
          max= x$tp,
          AC50 = x$AC50
        )
        Emax <- exp(Emax_resp$minimum)

        IA.eff <- calc_independent_action(
          x$C_invitro, x$tp, x$AC50, Emax
        )

        E10 <- Emax * 0.1

        EC10.result <- stats::optimize(
          obj_ECx,
          interval = c(-1000,1000),
          resp = E10,
          conc = x$C_invitro,
          max = x$tp,
          AC50 = x$AC50
        )
        EC10.GCA <- EC10.result$minimum

        E10.by.chem <- x$tp * 0.1
        AC10 <- hill_conc(E10.by.chem, x$tp, x$AC50, 1)

        sCi <- sum(x$C_invitro)
        GCA.HQ.10 <- if (EC10.GCA > 0) sCi / EC10.GCA else NA_real_
        IA.HQ.10 <- sum(x$C_invitro / AC10)

        c(GCA.eff, IA.eff, GCA.HQ.10, IA.HQ.10)
      })
    )

  df <- dplyr::bind_cols(
    sample_id = df$sample_id,
    as.data.frame(do.call(rbind, df$data))
  )
  colnames(df)[-1] <- c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10")
  tibble::as_tibble(df)
}
