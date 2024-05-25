#' Title
#'
#' @param x .
#' @param vary .
#' @param tp_b_mult .
#'
#' @return .
#' @export
compute_sensitivity <- function(
    x,
    vary = c("age", "obesity", "css_params", "fit_params", "C_ext"),
    tp_b_mult = NULL) {
  
  # TODO Should re-simulations be run every time or once and saved?
  # TODO should tp_b_mult be allowed to change?
  #      e.g. tp_b_mult <- ifelse(vary == "step 4", 1.2, 1.5)
  
  vary <- match.arg(vary)
  
  C_ss <- switch(vary,
                 age         = x$css_sensitivity$age,
                 obesity     = x$css_sensitivity$obesity,
                 css_params  = x$css_sensitivity$params,
                 x$css_sensitivity$other)
  
  if (is.null(tp_b_mult)) {
    tp_b_mult <- x$inputs$resp$tp_b_mult
  }
  
  if (vary == "age") {
    age <- x$age
    IR <- x$IR
  } else {
    age <- lapply(x$age, function(x) rep(stats::median(x),
                                         length.out = length(x)))
    IR <- simulate_inhalation_rate(age, IR_params = x$inputs$IR_params)
  }
  
  if (vary == "C_ext") {
    C_ext <- x$C_ext
  } else {
    # Set exposure sd = NA (or 0)
    exposure <- lapply(x$inputs$exposure$x, \(x) x |> dplyr::mutate(sd = NA))
    C_ext <- simulate_exposure(x           = exposure,
                               expos_mean  = x$inputs$exposure$expos_mean,
                               expos_sd    = x$inputs$exposure$expos_sd,
                               expos_label = x$inputs$exposure$expos_label,
                               n           = x$inputs$n)
  }
  
  D_int <- calc_internal_dose(C_ext,
                              IR,
                              time    = x$inputs$D_int$time,
                              BW      = x$inputs$D_int$BW,
                              scaling = x$inputs$D_int$scaling)
  C_invitro <- calc_invitro_concentration(D_int, C_ss)
  resp <- calc_concentration_response(C_invitro,
                                      x$inputs$hill_params,
                                      tp_b_mult = tp_b_mult,
                                      fixed     = vary != "fit_params")
  
  resp
}
