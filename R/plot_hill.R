#' Title
#'
#' @param fit_params x
#'
#' @return x
#' @importFrom rlang .data
#' @export
plot_hill <- function(fit_params) {
  
  if (is.null(fit_params)) {
    stop("No hill parameters found.", call. = FALSE)
  }
  
  log10_x <- seq(-1, 4, length.out = 100)
  
  y <- apply(fit_params, 1, function(par) {
    tp      <- as.numeric(par["tp"])
    logAC50 <- as.numeric(par["logAC50"])
    slope   <- as.numeric(par["slope"])
    tp / (1 + 10^(slope * (logAC50 - log10_x)))
  })
  colnames(y) <- fit_params$name
  
  tibble::as_tibble(y) |> 
    dplyr::mutate(conc = 10^log10_x, .before = 1) |> 
    tidyr::pivot_longer(-"conc", values_to = "resp") |> 
    ggplot2::ggplot(ggplot2::aes(.data$conc, .data$resp, color = .data$name)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_log10(guide = "axis_logticks",
                           labels = scales::label_number(drop0trailing = TRUE))
}