#' Plot GeoTox reponse data for multiple assays
#'
#' @param df multi-assay response tibble returned by [calculate_multi_response].
#' @title_plot title. Should be in the format of the total quantile followed by the assay quantile.
#' @return ggplot2 figure object.
#' @importFrom rlang .data .env
#' @importFrom sf st_as_sf
#' @export
plot_multi <- function(
    df, 
    title_label = c("10th percentile of assay median responses")) {
  

  if (all(is.na(df$value))) {
    limits <- c(0, 1)
  } else {
    limits <- c(0, max(df$value, na.rm = TRUE))
  }

  metric <- df$metric[1]


  fig <- ggplot2::ggplot(df, ggplot2::aes(fill = .data$value)) +
    # Plot county data using fill, hide county borders by setting color = NA
    ggplot2::geom_sf(ggplot2::aes(geometry = .data$geometry), color = NA) +
    # Add fill scale
    ggplot2::scale_fill_viridis_c(
      name = metric,
      direction = -1,
      option = "A",
      trans = "sqrt",
      limits = limits,
      na.value = "grey70"
    ) +
    # Theme
    ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 8),
      axis.ticks = ggplot2::element_line(color = "black"),
      axis.text = ggplot2::element_text(size = 10, color = "black"),
      panel.grid.major = ggplot2::element_line(color = "grey80"),
      panel.grid.minor = ggplot2::element_line(color = "grey90")
    )
  

  fig <- fig + ggplot2::labs(title = title_label)
  fig
}
