#' Calculate the mixture response from one of three different approaches: IA, GCA ,or Hazard Quotient
#'
#' @param resp data frame with columns "tp", "logAC50", "resp_max", "logc_min",
#' "logc_max".
#' @param concentration concentration
#'
#' @description 
#' Calculate the combined response of multiple chemicals. It calculates the generalized concentration addition response, the independent actionr response, and a hazard quotient
#' @return data frame
#' @export
calc_concentration_response <- function(resp, concentration) {

  # TODO add input for linear/log concentration
  # TODO maybe add option for ln(resp) or not

  interval <- c(-50,50)

  tp <- truncnorm::rtruncnorm(
    1,
    a    = 0,
    b    = resp$resp_max * 1.5,
    mean = resp$tp,
    sd   = 0 # resp$tp.sd
  )

  logAC50 <- truncnorm::rtruncnorm(
    1,
    a    = resp$logc_min - 2,
    b    = resp$logc_max + 0.5,
    mean = resp$logAC50,
    sd   = 0 # resp$logAC50.sd
  )

  GCA.eff <- IA.eff <- GCA.HQ.10 <-IA.HQ.10 <- rep(NA, nrow(concentration))
  for (i in 1:nrow(concentration)) {

    Ci <- concentration[i,]
    AC50 <- 10^logAC50
    mixture.result <- stats::optimize(
      obj_GCA, interval = interval, Ci = Ci, tp = tp, AC50 = AC50
    )
    GCA.eff[i] <- exp(mixture.result$minimum)

    # TODO replace with positive control value if given
    Emax_resp <- stats::optimize(
      obj_GCA, interval = interval, Ci = Ci * 10^14, tp = tp, AC50 = AC50
    )
    Emax <- exp(Emax_resp$minimum)

    IA.eff[i] <- calc_independent_action(Ci, tp, AC50, Emax)

    E10 <- Emax * 0.1

    EC10.result <- stats::optimize(
      obj_ECx, interval = c(-1000,1000), E = E10, Ci = Ci, tp = tp, AC50 = AC50
    )

    EC10.GCA <- EC10.result$minimum
    E10.by.chem <- tp * 0.1
    AC10.ij <- tcplHillConc(E10.by.chem, tp, AC50, 1)

    sCi <- sum(Ci)
    if (EC10.GCA > 0){
      GCA.HQ.10[i] <- sCi / EC10.GCA
    }
    IA.HQ.10[i]  <- sum(Ci / AC10.ij)

  }


  data.frame(
    "GCA.Eff" = GCA.eff, "IA.eff" = IA.eff,
    "GCA.HQ.10" = GCA.HQ.10, "IA.HQ.10" = IA.HQ.10
  )
}
