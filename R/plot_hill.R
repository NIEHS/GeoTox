#' Plot Hill equation fits.
#'
#' @param hill_params output from \code{\link{fit_hill}}.
#' @param xlim log-10 scaled concentration limits.
#'
#' @return ggplot2 object.
#' @export
#' 
#' @examples
#' # Multiple assays, multiple chemicals
#' df <- geo_tox_data$dose_response
#' plot_hill(fit_hill(df, assay = "endp", chem = "casn"))
#' 
#' # Single assay, multiple chemicals
#' df <- geo_tox_data$dose_response |>
#'   dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#' fig <- plot_hill(fit_hill(df, chem = "casn"))
#' fig
#' # Modify plot
#' fig + ggplot2::guides(color = ggplot2::guide_legend(title = "Chemical\nCASN"))
#' 
#' # Single assay, single chemical
#' df <- geo_tox_data$dose_response |>
#'   dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
#'                 casn == "510-15-6")
#' plot_hill(fit_hill(df))
#' # 3-parameter Hill model
#' plot_hill(fit_hill(df, fixed_slope = FALSE))
plot_hill <- function(hill_params, xlim = c(-1, 4)) {
  
  if (is.null(hill_params)) {
    stop("No Hill parameters found.", call. = FALSE)
  }
  
  log10_x <- seq(xlim[1], xlim[2], length.out = 100)
  cols <- c("assay", "chem", "x", "y")
  df <- hill_params |> 
    dplyr::rowwise() |> 
    dplyr::mutate(
      x = list(.env$log10_x),
      y = list(.data$tp / (1 + 10^(.data$slope * (.data$logAC50 - .data$x))))) |> 
    dplyr::select(tidyselect::any_of(cols)) |> 
    tidyr::unnest(cols = c("x", "y"))
  
  fig <- ggplot2::ggplot(df, ggplot2::aes(x = 10^.data$x, y = .data$y))
  if ("chem" %in% names(df)) {
    fig <- fig + ggplot2::geom_line(ggplot2::aes(color = .data$chem))
  } else {
    fig <- fig + ggplot2::geom_line()
  }
  if ("assay" %in% names(df)) {
    fig <- fig + ggplot2::facet_wrap(~assay)
  }
  fig +
    ggplot2::xlab("Concentration") +
    ggplot2::ylab("Response") +
    ggplot2::scale_x_log10(guide = "axis_logticks",
                           labels = scales::label_number(drop0trailing = TRUE))
}