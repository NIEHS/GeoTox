#' Plot GeoTox reponse data
#'
#' @param resp calculated mixture response from [calculate_response].
#' @param region_boundary "sf" data.frame mapping features to a "geometry"
#' column. Used to color map regions.
#' @param group_boundary "sf" data.frame containing a "geometry" column. Used
#' to draw outlines around groups of regions.
#' @param metric response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10"
#' or "IA.HQ.10".
#' @param assay assay to plot. If NULL and multiple assays exist, then the
#' first assay is used.
#' @param quantiles quantiles to plot.
#' @param quantile_labels labels for quantiles.
#'
#' @return ggplot2 figure object.
#' @importFrom rlang .data .env
#' @importFrom sf st_as_sf
#' @export
plot_resp <- function(
    resp,
    region_boundary,
    group_boundary = NULL,
    metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
    assay = NULL,
    quantiles = c(0.5),
    quantile_labels = c("Median")) {
  
  if (is.null(resp)) {
    stop("No response data found.", call. = FALSE)
  }
  
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
    tidyr::pivot_longer(cols = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
                        names_to = "metric") |> 
    dplyr::filter(.data$metric == .env$metric)
  if (is.null(assay) && "assay" %in% names(df)) {
    assay <- df$assay[[1]]
    warning(paste0("Multiple assays found, using first assay '", assay, "'"),
            call. = FALSE)
  }
  if (!is.null(assay) && !("assay" %in% names(df))) {
    stop("No assay data found.", call. = FALSE)
  }
  if (!is.null(assay)) {
    df <- dplyr::filter(df, .data$assay == .env$assay)
  }
  df <- df |> 
    dplyr::reframe(quantile = quantiles,
                   value = stats::quantile(.data$value, quantiles, na.rm = TRUE),
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
  
  if (all(is.na(df$value))) {
    limits <- c(0, 1)
  } else {
    limits <- c(0, max(df$value, na.rm = TRUE))
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
  
  if (!is.null(group_boundary)) {
    fig <- fig + ggplot2::geom_sf(data = group_boundary, fill = NA,
                                  size = 0.15)
  }
  
  if (!is.null(assay)) {
    fig <- fig + ggplot2::labs(title = assay)
  }
  
  fig
}
