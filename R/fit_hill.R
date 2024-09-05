#' Fit 2- or 3-parameter Hill model
#'
#' @param x data frame of dose response data.
#' @param conc column name of base-10 log scaled concentration.
#' @param resp column name of response.
#' @param fixed_slope if TRUE, slope is fixed at 1.
#' @param chem (optional) column name of chemical identifiers.
#' @param assay (optional) column name of assay identifiers.
#'
#' @details
#' Optional `chem` and `assay` identifiers can be used to fit multiple
#' chemicals and/or assays. Returned columns `tp` is the top asymptote and
#' `logAC50` is the 50% response concentration. If the computation of the
#' standard deviations of these two parameters fails, then the standard
#' deviation is set equal to the parameter estimate and is indicated by the
#' respective imputed flag being TRUE.
#' 
#' @return data frame of fit parameters.
#' @export
#'
#' @examples
#' # Multiple assays, multiple chemicals
#' df <- geo_tox_data$dose_response
#' fit_hill(df, assay = "endp", chem = "casn")
#' 
#' # Single assay, multiple chemicals
#' df <- geo_tox_data$dose_response |>
#'   dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#' fit_hill(df, chem = "casn")
#' 
#' # Single assay, single chemical
#' df <- geo_tox_data$dose_response |>
#'   dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
#'                 casn == "510-15-6")
#' fit_hill(df)
#' # 3-parameter Hill model
#' fit_hill(df, fixed_slope = FALSE)
fit_hill <- function(x, conc = "logc", resp = "resp", fixed_slope = TRUE,
                     chem = NULL, assay = NULL) {

  if (!inherits(x, "data.frame")) {
    stop("x must be a data.frame")
  }
  if (!all(c(conc, resp) %in% names(x))) {
    stop("x must contain columns named by 'conc' and 'resp' inputs")
  }
  if (!is.null(chem) && !chem %in% names(x)) {
    stop("x must contain a column named by 'chem' when not NULL")
  }
  if (!is.null(assay) && !assay %in% names(x)) {
    stop("x must contain a column named by 'assay' when not NULL")
  }
  
  cols <- c("assay" = assay, "chem" = chem, "x" = conc, "y" = resp)
  x |> 
    dplyr::select(tidyselect::any_of(cols)) |> 
    tidyr::nest(.by = tidyselect::any_of(c("assay", "chem")), .key = "df") |> 
    dplyr::mutate(
      df = purrr::map(.data$df, \(x) .fit_hill(x, "x", "y", fixed_slope)),
      df = purrr::map(.data$df, \(x) .extract_hill_params(x))) |> 
    tidyr::unnest("df") |> 
    dplyr::arrange(dplyr::across(tidyselect::any_of(c("assay", "chem"))))
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
