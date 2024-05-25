#' Title
#'
#' @param x .
#' @param param .
#' @param y .
#' @param xlab .
#' @param ylab .
#'
#' @return .
#' @importFrom rlang .data .env
#' @export
plot_sensitivity <- function(x,
                             param = "GCA.Eff",
                             y = "",
                             xlab = param,
                             ylab = "") {
  df <- get_sensitivity_df(x, param = param)
  plot_sensitivity_df(df, y = y, xlab = xlab, ylab = ylab)
}

get_sensitivity_df <- function(x,
                               param = c("GCA.Eff", "IA.Eff",
                                         "GCA.HQ.10", "IA.HQ.10")) {
  param <- match.arg(param)
  colnames <- c(
    "External Concentration",
    "Toxicokinetic Parameters",
    "Obesity",
    "Age",
    "Concentration-Response",
    "Baseline"
  )
  out <- cbind(
    unlist(lapply(x$sensitivity$C_ext,      "[[", param)),
    unlist(lapply(x$sensitivity$css_params, "[[", param)),
    unlist(lapply(x$sensitivity$obesity,    "[[", param)),
    unlist(lapply(x$sensitivity$age,        "[[", param)),
    unlist(lapply(x$sensitivity$fit_params, "[[", param)),
    unlist(lapply(x$resp,                   "[[", param))
  )
  colnames(out) <- colnames
  tibble::as_tibble(out) |> 
    tidyr::pivot_longer(cols = tidyr::everything()) |> 
    dplyr::mutate(name = factor(.data$name, levels = colnames))
}

plot_sensitivity_df <- function(df, y = "", xlab = "", ylab = "") {
  df |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$value,
                                 y = .env$y,
                                 fill = NA,
                                 color = .data$name)) +
    ggridges::stat_density_ridges(
      calc_ecdf = TRUE,
      quantiles = 4,
      quantile_lines = FALSE,
      fill = NA,
      linewidth = 1
    ) +
    ggplot2::scale_x_log10(guide = "axis_logticks") +
    ggplot2::scale_color_brewer(palette = "Set2") +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab) +
    ggplot2::labs(color = 'Varying Parameter') +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      text = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 14),
      axis.title = ggplot2::element_text(size = 14)
    )
}