#' @export
#' @rdname get_risk_values
get_risk_sensitivity <- function(
    GT, metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"), assay = NULL
) {
  metric <- match.arg(metric)
  tibble::tibble(
    "C_ext" = get_risk_values(
      GT, metric, assay, "risk_sensitivity_C_ext"
    ),
    "css_params" = get_risk_values(
      GT, metric, assay, "risk_sensitivity_css_params"
    ),
    "weight" = get_risk_values(
      GT, metric, assay, "risk_sensitivity_weight"
    ),
    "age" = get_risk_values(
      GT, metric, assay, "risk_sensitivity_age"
    ),
    "fit_params" = get_risk_values(
      GT, metric, assay, "risk_sensitivity_fit_params"
    ),
    "baseline" = get_risk_values(
      GT, metric, assay, "risk"
    )
  )
}
