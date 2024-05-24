#' Plot GeoTox reponse data
#'
#' @param resp .
#' @param region_boundary .
#' @param group_boundary .
#' @param metric response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10"
#' or "IA.HQ.10"
#' @param quantiles quantiles to plot
#' @param quantile_labels labels for quantiles
#'
#' @return ggplot2 figure object
#' @importFrom rlang .data .env
#' @export
plot_resp <- function(
    resp,
    region_boundary,
    group_boundary = NULL,
    metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
    quantiles = c(0.5),
    quantile_labels = c("Median")) {
  
  metric <- match.arg(metric)
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to use this function", call. = FALSE)
  }
  
  if (length(quantiles) != length(quantile_labels)) {
    stop("Length of 'quantiles' and 'quantile_labels' must be the same",
         call. = FALSE)
  }
  
  df <- tibble::tibble(id = names(resp), data = resp) |> 
    tidyr::unnest(cols = "data") |> 
    tidyr::pivot_longer(-"id", names_to = "metric") |> 
    dplyr::filter(.data$metric == .env$metric) |> 
    dplyr::reframe(quantile = quantiles,
                   value = stats::quantile(.data$value, quantiles),
                   .by = c("id", "metric")) |> 
    dplyr::inner_join(region_boundary |> dplyr::rename("id" = 1),
                      by = dplyr::join_by("id"))

  if (nrow(df) == 0) {
    stop("No spatial data for corresponding response data", call. = FALSE)
  }
  if (nrow(df) != length(resp) * length(quantiles)) {
    warning("Some response data was removed due to missing spatial data",
            call. = FALSE)
  }
  
  fig <- ggplot2::ggplot(df, ggplot2::aes(fill = .data$value)) +
    # Plot county data using fill, hide county borders by setting color = NA
    ggplot2::geom_sf(ggplot2::aes(geometry = .data$geometry), color = NA) +
    # Create separate plots for each stat
    ggplot2::facet_wrap(
      ~quantile,
      ncol = length(quantiles),
      labeller = ggplot2::labeller(
        quantile = stats::setNames(quantile_labels, quantiles)
      )
    ) +
    # Add fill scale
    ggplot2::scale_fill_viridis_c(
      name = metric,
      direction = -1,
      option = "A",
      trans = "sqrt",
      limits = c(0, max(df$value, na.rm = TRUE))
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
  
  if (!is.null(group_boundary)) {
    fig <- fig + ggplot2::geom_sf(data = group_boundary, fill = NA,
                                  size = 0.15)
  }
  
  fig
}
