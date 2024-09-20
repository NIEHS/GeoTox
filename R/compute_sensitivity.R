#' Compute response sensitivity to parameter variation.
#'
#' @param x GeoTox object.
#' @param vary which parameter to vary.
#' @param tp_b_mult input for [calc_concentration_response] step.
#'
#' @return output from [calc_concentration_response]
#' @export
compute_sensitivity <- function(x,
                                vary = c("age", "obesity", "css_params",
                                         "fit_params", "C_ext"),
                                tp_b_mult = NULL) {
  
  vary <- match.arg(vary)
  
  C_ss <- switch(vary,
                 age         = x$css_sensitivity$age,
                 obesity     = x$css_sensitivity$obesity,
                 css_params  = x$css_sensitivity$params,
                 x$css_sensitivity$other)
  
  if (is.null(tp_b_mult)) {
    tp_b_mult <- x$par$resp$tp_b_mult
  }
  
  if (vary == "age") {
    age <- x$age
    IR <- x$IR
  } else {
    age <- lapply(x$age, function(x) rep(stats::median(x),
                                         length.out = length(x)))
    IR <- simulate_inhalation_rate(age, IR_params = x$par$IR_params)
  }
  
  if (vary == "C_ext") {
    C_ext <- x$C_ext
  } else {
    # Set exposure sd = NA (or 0)
    exposure <- lapply(x$exposure, \(x) x |> dplyr::mutate(sd = NA))
    C_ext <- simulate_exposure(x           = exposure,
                               expos_mean  = x$par$exposure$expos_mean,
                               expos_sd    = x$par$exposure$expos_sd,
                               expos_label = x$par$exposure$expos_label,
                               n           = x$par$n)
  }
  
  D_int <- calc_internal_dose(C_ext,
                              IR,
                              time    = x$par$internal_dose$time,
                              BW      = x$par$internal_dose$BW,
                              scaling = x$par$internal_dose$scaling)
  C_invitro <- calc_invitro_concentration(D_int, C_ss)
  resp <- calc_concentration_response(C_invitro,
                                      x$hill_params,
                                      tp_b_mult = tp_b_mult,
                                      fixed     = vary != "fit_params")
  
  resp
}
