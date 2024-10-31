#' Get response quantiles
#'
#' @param resp calculated mixture response output from
#' [calc_concentration_response].
#' @param metric response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10"
#' or "IA.HQ.10".
#' @param assays assays to summarize. If NULL and multiple assays exist, then
#' the first assay is used.
#' @param assay_summary boolean indicating whether to summarize across assays.
#' @param assay_quantiles numeric vector of assay quantiles.
#' @param summary_quantiles numeric vector of quantiles to compute across all
#' assay quantiles.
#' 
#' @details
#' The columns of the returned data frame will vary based on the inputs. If
#' assays is specified and assay_summary is FALSE, then the resulting data
#' frame will have an assay column. If assay_summary is TRUE, then the data
#' frame will have an summary_quantile column.
#'
#' @return data frame with computed response quantiles.
#' @export
#' 
#' @examples
#' # Dummy response data
#' resp <- list(
#'   "r1" = data.frame(assay = c("a1", "a1", "a2", "a2"),
#'                     sample = c(1, 2, 1, 2),
#'                     GCA.Eff = c(1, 2, 3, 4),
#'                     IA.Eff = c(5, 6, 7, 8),
#'                     "GCA.HQ.10" = c(9, 10, 11, 12),
#'                     "IA.HQ.10" = c(13, 14, 15, 16)))
#'
#' # Summarize single assay
#' resp_quantiles(resp)
#' # Specify assay
#' resp_quantiles(resp, assays = "a1")
#' # Specify quantiles
#' resp_quantiles(resp, assays = "a1", assay_quantiles = c(0.25, 0.75))
#' # Specify metric
#' resp_quantiles(resp, assays = "a1", metric = "IA.HQ.10")
#' 
#' # Summarize across assays
#' resp_quantiles(resp, assay_summary = TRUE)
#' # Specify quantiles
#' suppressWarnings(
#'   resp_quantiles(resp,
#'                  assay_summary = TRUE,
#'                  assay_quantiles = c(0.25, 0.75),
#'                  summary_quantiles = c(0.1, 0.9))
#' )
resp_quantiles <- function(
    resp,
    metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
    assays = NULL,
    assay_summary = FALSE,
    assay_quantiles = c("Median" = 0.5),
    summary_quantiles = c("10th percentile" = 0.1)) {
  
  metric <- match.arg(metric)

  # Extract desired metric, collapse resp list into a single data frame
  df <- tibble::tibble(id = names(resp), resp_data = resp) |> 
    dplyr::mutate(
      resp_data = purrr::map(.data$resp_data, \(x) {
        dplyr::select(x, tidyselect::any_of(c("assay", "sample", .env$metric)))
      })) |> 
    tidyr::unnest(cols = "resp_data") |> 
    tidyr::pivot_longer(cols = .env$metric, names_to = "metric")
  
  if (isTRUE(assay_summary)) {
    
    if (!"assay" %in% names(df)) {
      stop("Multiple assays required when 'assay_summary' is TRUE",
           call. = FALSE)
    }
    
    # Filter by assays if specified
    if (!is.null(assays)) {
      df <- df |> dplyr::filter(.data$assay %in% .env$assays)
      if (nrow(df) == 0) {
        stop("No response data for given assays", call. = FALSE)
      }
    }
    
    # Give warning for small number of assays
    n <- length(unique(df$assay))
    if (n <= 10) {
      warning("There are only ", n, " assays.\nConsider using a larger ",
              "number of assays for a more robust analysis.",
              call. = FALSE)
    }
    
    df <- df |> 
      # First, compute quantiles for each assay
      dplyr::reframe(assay_quantile = assay_quantiles,
                     value = stats::quantile(.data$value,
                                             assay_quantiles,
                                             na.rm = TRUE),
                     .by = c("id", "assay", "metric")) |> 
      # Then, compute quantiles across all assays
      dplyr::reframe(summary_quantile = summary_quantiles,
                     value = stats::quantile(.data$value,
                                             summary_quantiles,
                                             na.rm = TRUE),
                     .by = c("id", "assay_quantile", "metric"))
  } else {
    
    # Assign assay if not specified and multiple assays exist
    if (is.null(assays) && "assay" %in% names(df)) {
      assays <- df$assay[[1]]
      warning("Multiple assays found, using first assay '", assays, "'",
              call. = FALSE)
    }
    
    if (!is.null(assays)) {
      if (!"assay" %in% names(df)) {
        # Stop if assays is specified but not found
        stop("No assay column found in response data.", call. = FALSE)
      } else {
        # Filter by assays if specified
        df <- df |> dplyr::filter(.data$assay %in% .env$assays)
        if (nrow(df) == 0) {
          stop("No response data for given assays", call. = FALSE)
        }
      }
    }
    
    # Compute quantiles for each assay
    df <- df |> 
      dplyr::reframe(assay_quantile = assay_quantiles,
                     value = stats::quantile(.data$value,
                                             assay_quantiles,
                                             na.rm = TRUE),
                     .by = tidyselect::any_of(c("id", "assay", "metric")))
  }
  
  df
}
