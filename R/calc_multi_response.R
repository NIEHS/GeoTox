#' Calculate the response from a multi-assay, multi-chemical GeoTox object 
#' 
#' @param x GeoTox object
#' @param metric response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10"
#' or "IA.HQ.10".
#' @param quant_total The quantile to use for the total or overall response. This corresponds to the quantile of the vector of assay responses. Default is 0.1.
#' @param quant_assay The quantile to use for the level 2 response. This corresponds to the quantile for each assay mixture response. Default is 0.5.
#'
#' @return A tibble with the response data
#' @importFrom rlang .data .env
#' @importFrom sf st_as_sf
#' @export
calc_multi_response <- function(x, quant_total = 0.1, quant_assay = 0.5) {
  

  if (is.null(x$resp)) {
    stop("No response data found.", call. = FALSE)
  }
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required to use this function", call. = FALSE)
  }
  

  # Get the respone and boundary data from the GeoTox object
   resp            <- x$resp
   region_boundary <- x$boundaries$region
   group_boundary  <- x$boundaries$group


    # Create a tibble with the response data and filter the metric
  df <- tibble::tibble(id = names(resp), data = resp) |> 
    tidyr::unnest(cols = "data") |> 
    tidyr::pivot_longer(cols = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
                        names_to = "metric") |> 
    dplyr::filter(.data$metric == .env$metric)

  
  n_assays <- length(unique(x$hill_params$assay))
  if (n_assays <= 10) {
    sprintf("There are only %s assays. Consider using a larger number of assays for a more robust analysis.",  n_assays)
  }

    df <- df |> 
    dplyr::reframe(quantile = quant_assay,
                   value = stats::quantile(.data$value, quant_assay, na.rm = TRUE),
                   .by = c("id", "assay", "metric")) |> 
    dplyr::summarise(value = stats::quantile(.data$value, quant_total, na.rm = TRUE),
                    .by = c("id", "metric")) |>
    dplyr::inner_join(region_boundary |> dplyr::rename("id" = 1),
                      by = dplyr::join_by("id"))
  
  return(df)
  
} 
