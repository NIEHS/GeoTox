#' Plot response data
#'
#' @param df output from [resp_quantiles].
#' @param region_boundary "sf" data.frame mapping features to a "geometry"
#' column. Used to color map regions.
#' @param group_boundary "sf" data.frame containing a "geometry" column. Used
#' to draw outlines around groups of regions.
#' @param assay_quantiles named numeric vector of assay quantile labels.
#' @param summary_quantiles named numeric vector of summary quantile labels.
#'
#' @return ggplot2 object.
#' @export
#' 
#' @examples
#' # Use example boundary data from package
#' region_boundary <- geo_tox_data$boundaries$county
#' group_boundary <- geo_tox_data$boundaries$state
#' n <- nrow(region_boundary)
#' 
#' # Single assay quantile
#' df <- data.frame(id = region_boundary$FIPS,
#'                  metric = "GCA.Eff",
#'                  assay_quantile = 0.5,
#'                  value = runif(n)^3)
#' # Default plot
#' plot_resp(df, region_boundary)
#' # Add group boundary, a state border in this case
#' plot_resp(df, region_boundary, group_boundary)
#' # Change quantile label
#' plot_resp(df, region_boundary, group_boundary,
#'           assay_quantiles = c("Q50" = 0.5))
#' 
#' # Multiple assay quantiles
#' df <- data.frame(id = rep(region_boundary$FIPS, 2),
#'                  metric = "GCA.Eff",
#'                  assay_quantile = rep(c(0.25, 0.75), each = n),
#'                  value = c(runif(n)^3, runif(n)^3 + 0.15))
#' plot_resp(df, region_boundary, group_boundary,
#'           assay_quantiles = c("Q25" = 0.25, "Q75" = 0.75))
#' 
#' # Summary quantiles
#' df <- data.frame(id = rep(region_boundary$FIPS, 4),
#'                  assay_quantile = rep(rep(c(0.25, 0.75), each = n), 2),
#'                  summary_quantile = rep(c(0.05, 0.95), each = n * 2),
#'                  metric = "GCA.Eff",
#'                  value = c(runif(n)^3, runif(n)^3 + 0.15,
#'                            runif(n)^3 + 0.7, runif(n)^3 + 0.85))
#' plot_resp(df, region_boundary, group_boundary,
#'           assay_quantiles = c("A_Q25" = 0.25, "A_Q75" = 0.75),
#'           summary_quantiles = c("S_Q05" = 0.05, "S_Q95" = 0.95))
plot_resp <- function(
    df,
    region_boundary,
    group_boundary = NULL,
    assay_quantiles = c("Median" = 0.5),
    summary_quantiles = c("10th percentile" = 0.1)) {
  
  # Check for quantile names
  if (is.null(names(assay_quantiles)) | is.null(names(summary_quantiles))) {
    stop("Both assay_quantiles and summary_quantiles must be named",
         call. = FALSE)
  }
  
  # Add regional boundaries
  nrow_before <- nrow(df)
  df <- df |> 
    dplyr::inner_join(region_boundary |> dplyr::rename("id" = 1),
                      by = dplyr::join_by("id"))
  nrow_after <- nrow(df)
  
  if (nrow_after == 0) {
    stop("No spatial data for corresponding response data", call. = FALSE)
  } else if (nrow_after != nrow_before) {
    warning("Some response data was removed due to missing spatial data",
            call. = FALSE)
  }

  if (all(is.na(df$value))) {
    limits <- c(0, 1)
  } else {
    limits <- c(0, max(df$value, na.rm = TRUE))
  }
  
  metric <- df$metric[1]
  
  fig <- ggplot2::ggplot() +
    # Plot county data using fill, hide county borders by setting color = NA
    ggplot2::geom_sf(data = df,
                     ggplot2::aes(fill = .data$value,
                                  geometry = .data$geometry),
                     color = NA) +
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
  
  if ("summary_quantile" %in% names(df)) {
    fig <- fig +
      # Create separate plots for each stat
      ggplot2::facet_grid(
        assay_quantile ~ summary_quantile,
        labeller = ggplot2::labeller(
          assay_quantile = stats::setNames(names(assay_quantiles),
                                           assay_quantiles),
          summary_quantile = stats::setNames(names(summary_quantiles),
                                             summary_quantiles)))
  } else if ("assay" %in% names(df)) {
    fig <- fig +
      # Create separate plots for each stat
      ggplot2::facet_grid(
        assay ~ assay_quantile,
        labeller = ggplot2::labeller(
          assay_quantile = stats::setNames(names(assay_quantiles),
                                           assay_quantiles)))
  } else {
    fig <- fig +
      # Create separate plots for each stat
      ggplot2::facet_wrap(
        ~assay_quantile,
        ncol = length(unique(df$assay_quantile)),
        labeller = ggplot2::labeller(
          assay_quantile = stats::setNames(names(assay_quantiles),
                                           assay_quantiles)))
  }
  
  if (!is.null(group_boundary)) {
    fig <- fig + 
      ggplot2::geom_sf(data = group_boundary,
                       ggplot2::aes(geometry = .data$geometry),
                       fill = NA,
                       size = 0.15)
  }
  
  fig
}
