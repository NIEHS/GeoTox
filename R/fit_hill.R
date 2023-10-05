#' Fit 2- or 3-parameter Hill model
#'
#' @param log10_conc base-10 log scale concentration
#' @param resp response
#'
#' @return fit and other stats
fit_hill <- function(log10_conc, resp, fixed_slope = TRUE) {

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

  if (fixed_slope) {
    lb <- c(0, log10_conc_min - 2, -Inf)
    ub <- c(1.2 * resp_max, log10_conc_max + 0.5, Inf)
  } else {
    lb <- c(0, log10_conc_min - 2, 0.3, -Inf)
    ub <- c(1.2 * resp_max, log10_conc_max + 0.5, 8, Inf)
  }

  # Fit data
  fit <- stats::optim(
    par_init,
    fn = obj_hill,
    method = "L-BFGS-B",
    log10_conc = log10_conc,
    resp = resp,
    lower = lb,
    upper = ub,
    hessian = TRUE,
    control = list(
      fnscale = -1,
      maxit = 10000
    )
  )

  # Return results
  out <- list(
    par = fit$par,
    sds = sqrt(diag(solve(-fit$hessian))),
    val = fit$value,
    convergence = fit$convergence,
    AIC = 2 * length(fit$par) - 2 * fit$value,
    logc_max = log10_conc_max,
    logc_min = log10_conc_min,
    resp_max = resp_max,
    resp_min = resp_min
  )
  if (fixed_slope) {
    names(out$par) <- names(out$sds) <- c("tp", "logAC50", "t-error")
  } else {
    names(out$par) <- names(out$sds) <- c("tp", "logAC50", "slope", "t-error")
  }

  out
}
