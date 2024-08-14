#' Plot GeoTox reponse data for multiple assays
#'
#' @param df multi-assay response tibble returned by [calculate_multi_response].
#' @quantile_labels list of quantile labels. Should be in the format of the total quantile followed by the assay quantile.
#' @return ggplot2 figure object.
#' @importFrom rlang .data .env
#' @importFrom sf st_as_sf
#' @export
plot_multi <- function(
    df, 
    quantile_labels = c("10th percentile of assay median response")) {
  

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
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
  


  fig
}
