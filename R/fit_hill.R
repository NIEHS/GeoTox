#' Fit 2- or 3-parameter Hill model
#'
#' @param x data frame or list of data frames.
#' @param conc column name of base-10 log scaled concentration.
#' @param resp column name of response.
#' @param fixed_slope if TRUE, slope is fixed at 1.
#'
#' @return Fit parameters and other stats. "tp" is the top asymptote and
#' "logAC50" is the 50% response concentration. If the computation of the
#' standard deviations of these two parameters fails, then the standard
#' deviation is set equal to the parameter estimate and is indicated by
#' the respective imputed flag being TRUE.
#'
#' @examples
#' # Single chemical
#' data <- geo_tox_data$dose_response[
#'   geo_tox_data$dose_response$casn == "1582-09-8",
#' ]
#' fit_hill(data)
#'
#' # Multiple chemicals
#' data <- split(geo_tox_data$dose_response, ~casn)
#' # 2-param
#' fit_hill(data)
#' # 3-param
#' fit_hill(data, fixed_slope = FALSE)
#'
#' @export
fit_hill <- function(x, conc = "logc", resp = "resp", fixed_slope = TRUE) {

  if (!any(c("data.frame", "list") %in% class(x))) {
    stop("x must be a data.frame or list")
  }

  if ("data.frame" %in% class(x)) {

    if (!all(c(conc, resp) %in% names(x))) {
      stop("x must contain columns named by 'conc' and 'resp' inputs")
    }

    fit <- .fit_hill(x, conc, resp, fixed_slope)
    .extract_hill_params(list(fit))

  } else {

    if (!all(sapply(x, function(df) all(c(conc, resp) %in% names(df))))) {
      stop("x data frames must contain columns named by 'conc' and 'resp' inputs")
    }

    fit <- lapply(x, function(df) .fit_hill(df, conc, resp, fixed_slope))
    out <- .extract_hill_params(fit)
    out <- cbind("name" = rownames(out), out)
    rownames(out) <- NULL
    # Have consistent output order
    out <- out[order(out$name), , drop = FALSE]
    out

  }
}

.fit_hill <- function(x, conc, resp, fixed_slope) {

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
    sds <- c(sds[1:2], 0, sds)
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

.extract_hill_params <- function(fits) {

  # Convert list into data frame
  df_params <- do.call(rbind, lapply(fits, function(fit) {
    fit_params <- as.data.frame(t(unlist(fit)))
    fit_params <- fit_params[c(
      "par.tp", "sds.tp", "par.logAC50", "sds.logAC50", "par.slope",
      "sds.slope", "logc_min", "logc_max", "resp_min", "resp_max", "AIC"
    )]
    names(fit_params)[c(1:6)] <- c(
      "tp", "tp.sd", "logAC50", "logAC50.sd", "slope", "slope.sd"
    )
    fit_params
  }))

  # Impute sd NAs with mean for tp and logAC50
  if (all(is.na(df_params$tp.sd))) {
    df_params$tp.sd.imputed <- FALSE
  } else {
    df_params$tp.sd.imputed <- is.na(df_params$tp.sd)
    idx <- df_params$tp.sd.imputed
    df_params$tp.sd[idx] <- df_params$tp[idx]
  }

  if (all(is.na(df_params$logAC50.sd))) {
    df_params$logAC50.sd.imputed <- FALSE
  } else {
    df_params$logAC50.sd.imputed <- is.na(df_params$logAC50.sd)
    idx <- df_params$logAC50.sd.imputed
    df_params$logAC50.sd[idx] <- df_params$logAC50[idx]
  }

  # Return
  df_params
}
