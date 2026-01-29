#' Fit 2- or 3-parameter Hill model
#'
#' Fit a 2-parameter (fixed slope) or 3-parameter (variable slope) Hill model to
#' concentration-response data.
#'
#' The input `x` data frame must contain columns specified by `conc` and `resp`
#' arguments representing the log10-transformed concentration and response,
#' respectively.
#'
#' Optional `assay` and `substance` identifiers can be named vectors that are
#' used to fit multiple substances and/or assays. For example, `assay = c(name =
#' "assay", "model")` would indicate that `x` contains both and assay name and
#' model. The `name = "assay"` part would rename the "assay" column in `x` to
#' "name" in the 'assay' table when aded with [add_hill_params()].
#'
#' Returned column 'tp' is the top asymptote and 'logAC50' is the 50% response
#' concentration. If the computation of the standard deviations of these two
#' parameters fails, then the standard deviation is set equal to the parameter
#' estimate and is indicated by the respective imputed flag being TRUE.
#'
#' @param x Data frame of dose response data.
#' @param conc Column name of base-10 log scaled concentration (default "logc").
#' @param resp Column name of response (default "resp").
#' @param fixed_slope Logical indicating whether to fit a 2-parameter (TRUE) or
#'   3-parameter (FALSE) Hill model (default TRUE).
#' @param assay Column name of assay identifier(s) (optional, default NULL).
#' @param substance Column name of substance identifier(s) (optional, default
#'   NULL).
#'
#' @returns A list with elements 'fit', 'assay', 'substance'. The 'fit' element
#'   is a data frame of fit parameters, while the 'assay' and 'substance'
#'   elements indicate the column names used for assay and substance
#'   identifiers, respectively.
#' @export
#' @seealso [add_hill_params()]
#'
#' @examples
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
#'
#' # Fit 2-parameter Hill model
#' fit_hill(
#'   hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
#' )
#'
#' # Fit 3-parameter Hill model
#' fit_hill(hill_df, assay = "assay", substance = "casn", fixed_slope = FALSE)
fit_hill <- function(
    x, conc = "logc", resp = "resp", fixed_slope = TRUE, assay = NULL,
    substance = NULL
) {
  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data.frame")
  }
  if (!all(c(conc, resp) %in% names(x))) {
    stop("`x` must contain columns named by 'conc' and 'resp' inputs")
  }
  if (!is.null(substance) && !all(substance %in% names(x))) {
    stop("`x` must contain column(s) named by 'substance' when not NULL")
  }
  if (!is.null(assay) && !all(assay %in% names(x))) {
    stop("`x` must contain column(s) named by 'assay' when not NULL")
  }

  assay_val <- unname(assay)
  substance_val <- unname(substance)
  cols <- c(assay_val, substance_val, "x" = conc, "y" = resp)

  fit <- x |>
    dplyr::select(tidyselect::all_of(cols)) |>
    tidyr::nest(
      .by = tidyselect::all_of(c(assay_val, substance_val)),
      .key = "df"
    ) |>
    dplyr::mutate(
      df = purrr::map(.data$df, \(x) .fit_hill(x, "x", "y", fixed_slope)),
      df = purrr::map(.data$df, \(x) .extract_hill_params(x))) |>
    tidyr::unnest("df") |>
    dplyr::arrange(dplyr::across(tidyselect::all_of(c(assay_val, substance_val))))

  list(
    fit = fit,
    assay = assay,
    substance = substance
  )
}

.fit_hill <- function(x, conc = "logc", resp = "resp", fixed_slope = TRUE) {

  log10_conc <- x[[conc]]
  resp <- x[[resp]]

  # Compute initial values
  resp_medians <- tapply(resp, log10_conc, stats::median)
  resp_mad <- stats::mad(resp)

  resp_init <- resp_medians[which.max(abs(resp_medians))]
  conc_init <- as.numeric(names(resp_init)) - 0.5
  err_init <- ifelse(resp_mad > 0, log(resp_mad), .Machine$double.eps)

  if (fixed_slope) {
    par_init <- c(resp_init, conc_init, err_init)
  } else {
    par_init <- c(resp_init, conc_init, 1.2, err_init)
  }

  # Determine bounds
  resp_max <- max(resp)
  resp_min <- min(resp)
  log10_conc_min <- min(log10_conc)
  log10_conc_max <- max(log10_conc)

  bounds <- as.data.frame(rbind(
    c(                 0,       1.2 * resp_max), # top asymptote
    c(log10_conc_min - 2, log10_conc_max + 0.5), # log10(AC50)
    c(               0.3,                    8), # slope
    c(              -Inf,                  Inf)  # err
  ))
  colnames(bounds) <- c("lower", "upper")

  if (fixed_slope) {
    # remove slope bound
    bounds <- bounds[-3, ]
  }

  # Fit data
  fit <- stats::optim(
    par_init,
    fn = obj_hill,
    method = "L-BFGS-B",
    log10_conc = log10_conc,
    resp = resp,
    lower = bounds$lower,
    upper = bounds$upper,
    hessian = TRUE,
    control = list(
      fnscale = -1,
      maxit = 10000
    )
  )

  sds <- suppressWarnings(sqrt(diag(solve(-fit$hessian))))
  if (fixed_slope) {
    par <- c(fit$par[1:2], 1, fit$par[3])
    sds <- c(sds[1:2], 0, sds[3])
  } else {
    par <- fit$par
  }

  # Return results
  out <- list(
    par = par,
    sds = sds,
    val = fit$value,
    convergence = fit$convergence,
    AIC = 2 * length(fit$par) - 2 * fit$value,
    logc_max = log10_conc_max,
    logc_min = log10_conc_min,
    resp_max = resp_max,
    resp_min = resp_min
  )
  names(out$par) <- names(out$sds) <- c("tp", "logAC50", "slope", "t-error")

  out
}

.extract_hill_params <- function(fit) {

  # Extract parameters
  cols <- c("tp" = "par.tp", "tp.sd" = "sds.tp",
            "logAC50" = "par.logAC50", "logAC50.sd" = "sds.logAC50",
            "slope" = "par.slope", "slope.sd" = "sds.slope",
            "logc_min", "logc_max", "resp_min", "resp_max", "AIC")
  params <- tibble::as_tibble(t(unlist(fit))) |>
    dplyr::select(tidyselect::all_of(cols))

  # Impute sd NAs with mean for tp and logAC50
  params |>
    dplyr::mutate(
      tp.sd.imputed = is.na(.data$tp.sd),
      tp.sd = dplyr::if_else(.data$tp.sd.imputed,
                             .data$tp,
                             .data$tp.sd),
      logAC50.sd.imputed = is.na(.data$logAC50.sd),
      logAC50.sd = dplyr::if_else(.data$logAC50.sd.imputed,
                                  .data$logAC50,
                                  .data$logAC50.sd))
}
