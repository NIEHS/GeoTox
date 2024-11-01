#' Plot exposure data.
#'
#' @param exposure list of exposure data named by region label.
#' @param region_boundary "sf" data.frame mapping features to a "geometry"
#' column. Used to color regions.
#' @param group_boundary (optional) "sf" data.frame containing a "geometry"
#' column. Used to draw outlines.
#' @param chem_label label for facet_wrap.
#' @param ncol number of columns to wrap.
#'
#' @return ggplot2 object.
#' @export
#' 
#' @examples
#' # Load package data
#' exposure <- split(geo_tox_data$exposure, ~FIPS)
#' region_boundary <- geo_tox_data$boundaries$county
#' group_boundary <- geo_tox_data$boundaries$state
#' 
#' # Plot county exposure data
#' plot_exposure(exposure,
#'               region_boundary,
#'               ncol = 5)
#'
#' # Add state boundaries
#' plot_exposure(exposure,
#'               region_boundary,
#'               group_boundary = group_boundary,
#'               ncol = 5)
#'
#' # Change facet strip label source
#' plot_exposure(exposure,
#'               region_boundary,
#'               group_boundary = group_boundary,
#'               chem_label = "casn",
#'               ncol = 5)
plot_exposure <- function(exposure,
                          region_boundary,
                          group_boundary = NULL,
                          chem_label = "chnm",
                          ncol = 2) {
  
  if (is.null(exposure)) {
    stop("No exposure data found.", call. = FALSE)
  }
  if (is.null(region_boundary)) {
    stop("No region_boundary data found.", call. = FALSE)
  }
  
  df <- tibble::tibble("_temp_join_id_" = names(exposure), data = exposure) |> 
    tidyr::unnest(cols = "data") |> 
    dplyr::inner_join(region_boundary |> dplyr::rename("_temp_join_id_" = 1),
                      by = dplyr::join_by("_temp_join_id_")) |> 
    dplyr::select(-"_temp_join_id_") |> 
    # Fix for grid.Call error in examples due to
    # 'mbcsToSbcs': for ​ (U+200B)
    # Remove any zero-width space characters
    dplyr::mutate(dplyr::across(tidyselect::all_of(chem_label),
                                ~ stringr::str_remove_all(., "\u200b")))
  
  fig <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = df,
      ggplot2::aes(fill = .data$norm,
                   geometry = .data$geometry),
      color = NA) +
    ggplot2::facet_wrap(chem_label, ncol = ncol) +
    # Recolor subset as light grey
    ggplot2::geom_sf(
      data = df |> dplyr::filter(mean == 0),
      ggplot2::aes(geometry = .data$geometry),
      fill = "light grey",
      color = "light grey",
      lwd = 0.01)
  
  # State borders
  if (!is.null(group_boundary)) {
    fig <- fig +
      ggplot2::geom_sf(data = group_boundary,
                       ggplot2::aes(geometry = .data$geometry),
                       fill = NA,
                       size = 0.15)
  }
  
  fig +
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
