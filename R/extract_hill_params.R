#' Extract results from 2-parameter fit_hill outputs
#'
#' @param fits list of outputs from fit_hill(..., fixed_slope = TRUE)
#'
#' @return data frame
#' @export
#'
#' @examples
#' df <- data.frame(
#'   logc = -3:3,
#'   resp = 5 / (1 + 10^(1.2 * (0.4 - rep(-3:3, each = 3)))) + rnorm(21)
#'  )
#' fit <- fit_hill(df$logc, df$resp)
#' extract_hill_params(list(fit))
#'
#' df_list <- list(df, df)
#' fits <- lapply(df_list, function(df) {fit_hill(df$logc, df$resp)})
#' extract_hill_params(fits)
extract_hill_params <- function(fits) {

  # TODO add ability to extract from 3-param fits?

  # Convert list into data frame
  df_params <- do.call(rbind, lapply(fits, function(fit) {
    fit_params <- as.data.frame(t(unlist(fit)))
    fit_params <- fit_params[c(
      "par.tp", "sds.tp", "par.logAC50", "sds.logAC50",
      "logc_min", "logc_max", "resp_min", "resp_max", "AIC"
    )]
    names(fit_params)[c(1:4)] <- c("tp", "tp.sd", "logAC50", "logAC50.sd")
    fit_params
  }))

  # Impute sd NAs with mean
  df_params$tp.sd.imputed <- is.na(df_params$tp.sd)
  idx <- df_params$tp.sd.imputed
  df_params$tp.sd[idx] <- df_params$tp[idx]

  df_params$logAC50.sd.imputed <- is.na(df_params$logAC50.sd)
  idx <- df_params$logAC50.sd.imputed
  df_params$logAC50.sd[idx] <- df_params$logAC50[idx]

  # Return
  df_params
}
