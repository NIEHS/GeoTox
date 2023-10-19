#' Extract results from 2-parameter fit_hill outputs
#'
#' @param fits list of outputs from fit_hill(..., fixed_slope = TRUE)
#'
#' @return data frame
#' @export
#'
#' @examples
#' fit <- fit_hill(df$logc, df$resp)
#' extract_hill_params(list(fit))
#'
#' fits <- lapply(df_list, function(df) {fit_hill(df$logc, df$resp)})
#' extract_hill_params(fits)
extract_hill_params <- function(fits) {

  # TODO add ability to extract from 3-param fits?

  # Convert list into data frame
  df_params <- do.call(
    rbind,
    lapply(fits, function(fit) {
      tibble::as_tibble(t(unlist(fit))) %>%
        dplyr::rename(
          tp         = par.tp,
          tp.sd      = sds.tp,
          logAC50    = par.logAC50,
          logAC50.sd = sds.logAC50
        ) %>%
        dplyr::select(
          tp, tp.sd, logAC50, logAC50.sd,
          logc_min, logc_max, resp_min, resp_max, AIC
        ) %>%
        dplyr::mutate(dplyr::across(tp:AIC, ~ as.numeric(.x)))
    })
  )

  # Impute sd NAs with mean
  df_params %>%
    dplyr::mutate(
      tp.sd.imputed      = is.na(tp.sd),
      logAC50.sd.imputed = is.na(logAC50.sd),
      tp.sd      = dplyr::if_else(is.na(tp.sd), tp, tp.sd),
      logAC50.sd = dplyr::if_else(is.na(logAC50.sd), logAC50, logAC50.sd)
    )
}
