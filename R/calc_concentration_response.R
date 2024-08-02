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

  if (!any(c("matrix", "list") %in% class(C_invitro))) {
    stop("C_invitro must be a matrix or list")
  }
  if (!is.list(C_invitro)) C_invitro <- list(C_invitro)
  
  # Split hill_params by assay
  if ("assay" %in% names(hill_params)) {
    hill_params <- split(hill_params, ~assay)
  } else {
    hill_params <- list(hill_params)
  }
  
  # Calculate response for each assay
  lapply(C_invitro, \(C_invitro_i) {
    lapply(hill_params, \(hill_params_j) {
      if (ncol(C_invitro_i) == 1 & nrow(hill_params_j) == 1) {
        .calc_concentration_response(C_invitro_i, hill_params_j, tp_b_mult, fixed)
      } else {
        if (!"chem" %in% names(hill_params_j)) {
          stop("'hill_params' must contain a 'chem' column", call. = FALSE)
        }
        chems <- hill_params_j$chem
        if (!all(chems %in% colnames(C_invitro_i))) {
          stop("'hill_params' chemicals missing in 'C_invitro'", call. = FALSE)
        }
        C_invitro_i <- C_invitro_i[, chems, drop = FALSE]
        res <- .calc_concentration_response(C_invitro_i,
                                            hill_params_j,
                                            tp_b_mult,
                                            fixed) |> 
          dplyr::mutate(sample = dplyr::row_number(), .before = 1)
        if ("assay" %in% names(hill_params_j)) {
          res <- res |> 
            dplyr::mutate(assay = hill_params_j$assay[[1]], .before = 1)
        }
        res
      }
    }) |> 
      dplyr::bind_rows()
  })
}

.calc_concentration_response <- function(
    C_invitro, hill_params, tp_b_mult, fixed
) {

  interval <- c(-50,50)

  # TODO value of b not consistent
  # grep "tp.sim <-" ~/github/GeoToxMIE/*.R
  tp <- lapply(1:nrow(C_invitro), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = 0,
      b    = hill_params$resp_max * tp_b_mult,
      mean = hill_params$tp,
      sd   = if (fixed) 0 else hill_params$tp.sd
    )
  }) |> unlist()
  tp <- matrix(tp, nrow = nrow(C_invitro), byrow = TRUE)
  # Replace NAs with 0 (can occur when tp and tp.sd are 0)
  tp[is.na(tp)] <- 0

  logAC50 <- lapply(1:nrow(C_invitro), function(x) {
    truncnorm::rtruncnorm(
      1,
      a    = hill_params$logc_min - 2.0001,
      b    = hill_params$logc_max + 0.5001,
      mean = hill_params$logAC50,
      sd   = if (fixed) 0 else hill_params$logAC50.sd
    )
  }) |> unlist()
  logAC50 <- matrix(logAC50, nrow = nrow(C_invitro), byrow = TRUE)

  GCA.eff <- IA.eff <- GCA.HQ.10 <- IA.HQ.10 <- rep(NA, nrow(C_invitro))
  for (i in 1:nrow(C_invitro)) {

    C_i <- C_invitro[i, ]
    tp_i <- tp[i, ]
    AC50_i <- 10^logAC50[i, ]
    
    if (all(is.na(C_i) | C_i == 0)) {
      GCA.eff[i] <- IA.eff[i] <- GCA.HQ.10[i] <- IA.HQ.10[i] <- NA
      next
    } else {
      idx <- which(C_i > 0)
      C_i <- C_i[idx]
      tp_i <- tp_i[idx]
      AC50_i <- AC50_i[idx]
    }

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
