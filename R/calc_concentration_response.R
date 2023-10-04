#' Calculate the mixture response from one of three different approaches:
#' IA, GCA ,or Hazard Quotient
#'
#' @param resp data frame with columns "tp", "tp.sd", "logAC50", "logAC50.sd",
#' "resp_max", "logc_min", "logc_max".
#' @param concentration concentration
#' @param tp_b_mult upper bound multiplier for tp rtruncnorm
#' @param fixed if TRUE, sd = 0
#'
#' @description
#' Calculate the combined response of multiple chemicals. It calculates the
#' generalized concentration addition response, the independent action
#' response, and a hazard quotient
#' @return data frame
#' @export
calc_concentration_response <- function(
    resp, concentration, tp_b_mult = 1.5, fixed = FALSE
) {

  # TODO make inputs more general
  # TODO add input for linear/log concentration
  # TODO maybe add option for ln(resp) or not

  interval <- c(-50,50)

  # TODO value of b not consistent
  # grep "tp.sim <-" ~/github/GeoToxMIE/*.R
  tp <- t(sapply(1:nrow(concentration), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = 0,
      b    = resp$resp_max * tp_b_mult,
      mean = resp$tp,
      sd   = if (fixed) 0 else resp$tp.sd
    )
  }))

  logAC50 <- t(sapply(1:nrow(concentration), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = resp$logc_min - 2,
      b    = resp$logc_max + 0.5,
      mean = resp$logAC50,
      sd   = if (fixed) 0 else resp$logAC50.sd
    )
  }))

  GCA.eff <- IA.eff <- GCA.HQ.10 <-IA.HQ.10 <- rep(NA, nrow(concentration))
  for (i in 1:nrow(concentration)) {

    C_i <- concentration[i, ]
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
    if (EC10.GCA > 0){
      GCA.HQ.10[i] <- sCi / EC10.GCA
    }
    IA.HQ.10[i]  <- sum(C_i / AC10.ij)

  }


  data.frame(
    "GCA.Eff" = GCA.eff, "IA.eff" = IA.eff,
    "GCA.HQ.10" = GCA.HQ.10, "IA.HQ.10" = IA.HQ.10
  )
}
