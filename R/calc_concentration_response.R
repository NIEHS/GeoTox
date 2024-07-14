#' Calculate the mixture response from one of three different approaches:
#' IA, GCA ,or Hazard Quotient
#'
#' @param C_invitro in vitro concentrations
#' @param hill_params output from `fit_hill()`
#' @param tp_b_mult upper bound multiplier for tp rtruncnorm
#' @param fixed if TRUE, sd = 0
#'
#' @description
#' Calculate the combined response of multiple chemicals. It calculates the
#' generalized concentration addition response, the independent action
#' response, and a hazard quotient
#'
#' @return data frame
#'
#' @export
calc_concentration_response <- function(C_invitro,
                                        hill_params,
                                        tp_b_mult = 1.5,
                                        fixed = FALSE) {

  if (inherits(C_invitro, "matrix")) {
    .calc_concentration_response(C_invitro, hill_params, tp_b_mult, fixed)
  } else {
    mapply(
      .calc_concentration_response,
      C_invitro = C_invitro,
      hill_params = list(hill_params),
      tp_b_mult = tp_b_mult,
      fixed = fixed,
      SIMPLIFY = FALSE
    )
  }

}

.calc_concentration_response <- function(
    C_invitro, hill_params, tp_b_mult, fixed
) {

  interval <- c(-50,50)

  # TODO value of b not consistent
  # grep "tp.sim <-" ~/github/GeoToxMIE/*.R
  tp <- t(sapply(1:nrow(C_invitro), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = 0,
      b    = hill_params$resp_max * tp_b_mult,
      mean = hill_params$tp,
      sd   = if (fixed) 0 else hill_params$tp.sd
    )
  }))

  logAC50 <- t(sapply(1:nrow(C_invitro), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = hill_params$logc_min - 2.0001,
      b    = hill_params$logc_max + 0.5001,
      mean = hill_params$logAC50,
      sd   = if (fixed) 0 else hill_params$logAC50.sd
    )
  }))

  GCA.eff <- IA.eff <- GCA.HQ.10 <- IA.HQ.10 <- rep(NA, nrow(C_invitro))
  for (i in 1:nrow(C_invitro)) {

    C_i <- C_invitro[i, ]
    tp_i <- tp[i, ]
    AC50_i <- 10^logAC50[i, ]

    mixture.result <- stats::optimize(
      obj_GCA, interval = interval, Ci = C_i, tp = tp_i, AC50 = AC50_i
    )
    GCA.eff[i] <- exp(mixture.result$minimum)

    # TODO replace with positive control value if given
    Emax_resp <- stats::optimize(
      obj_GCA, interval = interval, Ci = C_i * 10^14, tp = tp_i, AC50 = AC50_i
    )
    Emax <- exp(Emax_resp$minimum)

    IA.eff[i] <- calc_independent_action(C_i, tp_i, AC50_i, Emax)

    E10 <- Emax * 0.1

    EC10.result <- stats::optimize(
      obj_ECx, interval = c(-1000,1000), E = E10, Ci = C_i, tp = tp_i, AC50 = AC50_i
    )

    EC10.GCA <- EC10.result$minimum
    E10.by.chem <- tp_i * 0.1
    AC10.ij <- tcplHillConc(E10.by.chem, tp_i, AC50_i, 1)

    sCi <- sum(C_i)
    if ( EC10.GCA > 0 ) {
      GCA.HQ.10[i] <- sCi / EC10.GCA
    }
    IA.HQ.10[i]  <- sum(C_i / AC10.ij)

  }


  data.frame(
    "GCA.Eff" = GCA.eff, "IA.Eff" = IA.eff,
    "GCA.HQ.10" = GCA.HQ.10, "IA.HQ.10" = IA.HQ.10
  )
}
