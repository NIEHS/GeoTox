#' Plot results of sensitivity analysis.
#'
#' @param x GeoTox object.
#' @param metric metric to plot. Valid choices are "GCA.Eff", "IA.Eff",
#' "GCA.HQ.10", and "IA.HQ.10".
#' @param assay which assay to plot, if multiple exist.
#' @param y y value or text for bottom of ridge plot.
#' @param xlab x-axis label.
#' @param ylab y-axis label.
#'
#' @return ggplot2 object.
#' @export
plot_sensitivity <- function(x,
                             metric = "GCA.Eff",
                             assay = NULL,
                             y = "",
                             xlab = metric,
                             ylab = "") {
  
  if (is.null(x$sensitivity)) {
    stop("No sensitivity data found.", call. = FALSE)
  }
  if (is.null(x$resp)) {
    stop("No baseline response data found.", call. = FALSE)
  }
  
  df <- get_sensitivity_df(x, metric = metric, assay = assay)
  fig <- plot_sensitivity_df(df, y = y, xlab = xlab, ylab = ylab)
  if (!is.null(assay)) {
    fig <- fig + ggplot2::ggtitle(assay)
  }
  fig
}

get_sensitivity_df <- function(x,
                               metric = c("GCA.Eff", "IA.Eff",
                                          "GCA.HQ.10", "IA.HQ.10"),
                               assay = NULL) {
  metric <- match.arg(metric)
  colnames <- c(
    "External Concentration",
    "Toxicokinetic Parameters",
    "Obesity",
    "Age",
    "Concentration-Response",
    "Baseline"
  )
  if (is.null(assay) && "assay" %in% names(x$sensitivity[[1]][[1]])) {
    assay <- x$sensitivity[[1]][[1]]$assay[[1]]
    warning(paste0("Multiple assays found, using first assay '", assay, "'"),
            call. = FALSE)
  }
  get_metric <- function(df, metric, assay) {
    if (!is.null(assay)) {
      df <- dplyr::filter(df, .data$assay == .env$assay)
    }
    df[[metric]]
  }
  out <- cbind(
    unlist(lapply(x$sensitivity$C_ext,
                  get_metric, metric = metric, assay = assay)),
    unlist(lapply(x$sensitivity$css_params,
                  get_metric, metric = metric, assay = assay)),
    unlist(lapply(x$sensitivity$obesity,
                  get_metric, metric = metric, assay = assay)),
    unlist(lapply(x$sensitivity$age,
                  get_metric, metric = metric, assay = assay)),
    unlist(lapply(x$sensitivity$fit_params,
                  get_metric, metric = metric, assay = assay)),
    unlist(lapply(x$resp,
                  get_metric, metric = metric, assay = assay)))
  colnames(out) <- colnames
  tibble::as_tibble(out) |> 
    tidyr::pivot_longer(cols = tidyr::everything()) |> 
    dplyr::mutate(name = factor(.data$name, levels = colnames))
}

plot_sensitivity_df <- function(df, y = "", xlab = "", ylab = "") {
  idx <- is.na(df$value)
  if (any(idx)) {
    warning(paste("Removed", sum(idx), "NA values"), call. = FALSE)
    df <- df |> dplyr::filter(!idx)
  }
  if (nrow(df) == 0) {
    stop("No data to plot", call. = FALSE)
  }
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
