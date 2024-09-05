#' Title
#'
#' @param exposure .
#' @param region_boundary .
#' @param group_boundary .
#' @param chem_label .
#' @param ncol .
#'
#' @return .
#' @export
plot_exposure <- function(exposure,
                          region_boundary,
                          group_boundary = NULL,
                          chem_label = "chnm",
                          ncol = 2) {
  
  if (is.null(exposure)) {
    stop("No exposure data found.", call. = FALSE)
  }
  
  df <- tibble::tibble(id = names(exposure), data = exposure) |> 
    tidyr::unnest(cols = "data") |> 
    dplyr::inner_join(region_boundary |> dplyr::rename("id" = 1),
                      by = dplyr::join_by("id"))
  
  ggplot2::ggplot(df, ggplot2::aes(fill = .data$norm)) +
    ggplot2::geom_sf(ggplot2::aes(geometry = .data$geometry), color = NA) +
    ggplot2::facet_wrap(chem_label, ncol = ncol) +
    # Recolor subset as light grey
    ggplot2::geom_sf(
      data = df |> dplyr::filter(mean == 0),
      ggplot2::aes(geometry = .data$geometry),
      fill = "light grey",
      color = "light grey",
      lwd = 0.01) +
    # State borders
    ggplot2::geom_sf(data = group_boundary, fill = NA, size = 0.15) +
    ggplot2::scale_fill_viridis_c(
      name = "Normalized\nConcentration",
      direction = -1,
      option = "A",
      limits = c(0, max(df$norm, na.rm = TRUE))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 6),
      text = ggplot2::element_text(size = 12),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank())
}