#' GeoTox S3 object
#'
#' @description
#' An S3 object that can be used to help organize the data and results of a
#' GeoTox analysis.
#'
#' @return a GeoTox S3 object
#' @export
#'
#' @examples
#' # See the vignette for a full example:
#' #   vignette("introduction", package = "geotox")
#'
#' \dontrun{
#' # Use a subset of the package data for demonstration purposes
#' set.seed(2357)
#' n <- 10 # Population size
#' m <- 2 # Number of regions
#' idx <- if (m < 100) sample(1:100, m) else 1:100
#'
#' geoTox <- GeoTox() |>
#'   # Set region and group boundaries (for plotting)
#'   set_boundaries(region = geo_tox_data$boundaries$county,
#'                  group  = geo_tox_data$boundaries$state) |>
#'   # Simulate populations for each region
#'   simulate_population(age           = split(geo_tox_data$age, ~FIPS)[idx],
#'                       obesity       = geo_tox_data$obesity[idx, ],
#'                       exposure      = split(geo_tox_data$exposure, ~FIPS)[idx],
#'                       simulated_css = geo_tox_data$simulated_css,
#'                       n             = n) |>
#'   # Estimated Hill parameters
#'   set_hill_params(geo_tox_data$dose_response |>
#'                     fit_hill(assay = "endp", chem = "casn") |>
#'                     dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
#'   # Calculate response
#'   calculate_response() |>
#'   # Perform sensitivity analysis
#'   sensitivity_analysis()
#'
#' # Print GeoTox object
#' geoTox
#'
#' # Plot hill fits
#' plot(geoTox, type = "hill")
#' # Plot exposure data
#' plot(geoTox, type = "exposure", ncol = 5)
#' # Plot response data
#' plot(geoTox, assays = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#' # Plot sensitivity data
#' plot(geoTox,
#'      type = "sensitivity",
#'      assay = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#' }
GeoTox <- function() {
  structure(
    list(
      par = list(
        n = 1e3,
        IR_params = NULL,
        obesity = list(obes_prev  = "OBESITY_CrudePrev",
                       obes_sd    = "OBESITY_SD",
                       obes_label = "FIPS"),
        exposure = list(expos_mean  = "mean",
                        expos_sd    = "sd",
                        expos_label = "casn"),
        internal_dose = list(time    = 1,
                             BW      = 1,
                             scaling = 1),
        resp = list(max_mult = 1.5)
      )
    ),
    class = "GeoTox")
}

#' @export
print.GeoTox <- function(x, ...) {

  # Get n_assay and n_chem from GeoTox()$hill_params
  if (is.null(x$hill_params)) {
    n_assay <- 0
    n_chem <- 0
  } else {
    if ("assay" %in% names(x$hill_params)) {
      n_assay <- length(unique(x$hill_params$assay))
    } else {
      n_assay <- 1
    }
    if ("chem" %in% names(x$hill_params)) {
      n_chem <- length(unique(x$hill_params$chem))
    } else {
      n_chem <- 1
    }
  }

  # Categorize different GeoTox() fields
  names_data_vec      <- c("age", "IR", "obesity")
  names_data_mat      <- c("C_ext", "C_ss")
  names_computed_mat  <- c("D_int", "C_invitro")
  names_computed_df   <- c("resp")
  names_computed_list <- c("sensitivity")
  names_other <- setdiff(names(x),
                         c(names_data_vec, names_data_mat,
                           names_computed_mat, names_computed_df,
                           names_computed_list))

  # Functions to get size info for each type of field
  # m = number of regions
  # n = population size
  get_info_vec <- function(name) {
    size <- ifelse(is.null(x[[name]]), "", "m * (n)")
    data.frame(Name = name, Size = size)
  }
  get_info_mat <- function(name) {
    size <- ""
    if (!is.null(x[[name]])) {
      dim <- dim(x[[name]][[1]])
      size <- paste0("m * (n x ", dim[2], ")")
    }
    data.frame(Name = name, Size = size)
  }
  get_info_df <- function(name) {
    size <- ""
    if (!is.null(x[[name]])) {
      dim <- dim(x[[name]][[1]])
      size <- paste0("m * (", n_assay, " * n x ", dim[2], ")")
    }
    data.frame(Name = name, Size = size)
  }
  get_info_list <- function(name) {
    size <- ""
    if (!is.null(x[[name]])) {
      n_list <- length(x[[name]])
      dim <- dim(x[[name]][[1]][[1]])
      size <- paste0(n_list, " * (m * (", n_assay, " * n x ", dim[2], "))")
    }
    data.frame(Name = name, Size = size)
  }

  # Get size info for each type of field
  info_data <- dplyr::bind_rows(
    purrr::map(names_data_vec, \(name) get_info_vec(name)),
    purrr::map(names_data_mat, \(name) get_info_mat(name))) |>
    dplyr::filter(.data$Size != "")

  info_computed <- dplyr::bind_rows(
    purrr::map(names_computed_mat,  \(name) get_info_mat(name)),
    purrr::map(names_computed_df,   \(name) get_info_df(name)),
    purrr::map(names_computed_list, \(name) get_info_list(name))) |>
    dplyr::filter(.data$Size != "")

  # Get population size from GeoTox()$par$n
  if (is.null(x$par$n)) {
    n_pop <- 0
  } else if (length(unique(x$par$n)) == 1) {
    n_pop <- x$par$n[[1]]
  } else {
    n_pop <- paste0("[", paste(range(x$par$n), collapse = ", "), "]")
  }

  # Get number of regions from potential data fields
  n_region <- purrr::map_int(c(names_data_vec, names_data_mat,
                               names_computed_mat, names_computed_df),
                             \(name) length(x[[name]])) |>
    max()

  # Output info
  cat("GeoTox object\n")
  cat("Assays: ", n_assay, "\n", sep = "")
  cat("Chemicals: ", n_chem, "\n", sep = "")
  cat("Regions: m = ", n_region, "\n", sep = "")
  cat("Population: n = ", n_pop, "\n", sep = "")
  cat("Data Fields:")
  if (nrow(info_data) > 0) {
    cat("\n")
    print(info_data, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Computed Fields:")
  if (nrow(info_computed) > 0) {
    cat("\n")
    print(info_computed, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Other Fields:")
  if (length(names_other) > 0) {
    cat(" ", paste(names_other, collapse = ", "), "\n", sep = "")
  } else {
    cat(" None\n")
  }
}

#' @rdname GeoTox
#'
#' @param x GeoTox object.
#' @param type type of plot.
#' @param ... arguments passed to subsequent methods.
#'
#' @seealso [plot_resp], [plot_hill], [plot_exposure], [plot_sensitivity]
#' @export
plot.GeoTox <- function(x,
                        type = c("resp", "hill", "exposure", "sensitivity"),
                        ...) {
  type <- match.arg(type)
  if (type == "resp") {
    if (is.null(x$resp)) {
      stop("No response data found.", call. = FALSE)
    }
    if (is.null(x$boundaries$region)) {
      stop("No region boundary data found.", call. = FALSE)
    }
    dots = list(...)
    metric = dots$metric %||% "GCA.Eff"
    assays = dots$assays
    assay_quantiles = dots$assay_quantiles %||% c("Median" = 0.5)
    assay_summary = dots$assay_summary %||% FALSE
    summary_quantiles = dots$summary_quantiles %||% c("10th percentile" = 0.1)
    df <- resp_quantiles(x$resp,
                         metric = metric,
                         assays = assays,
                         assay_summary = assay_summary,
                         assay_quantiles = assay_quantiles,
                         summary_quantiles = summary_quantiles)
    plot_resp(df,
              region_boundary = x$boundaries$region,
              group_boundary = x$boundaries$group,
              assay_quantiles = assay_quantiles,
              summary_quantiles = summary_quantiles)
  } else if (type == "hill") {
    plot_hill(x$hill_params,
              ...)
  } else if (type == "exposure") {
    dots = list(...)
    chem_label = dots$chem_label %||% x$par$exposure$expos_label
    ncol = dots$ncol %||% 2
    plot_exposure(x$exposure,
                  region_boundary = x$boundaries$region,
                  group_boundary  = x$boundaries$group,
                  chem_label      = chem_label,
                  ncol            = ncol)
  } else if (type == "sensitivity") {
    plot_sensitivity(x, ...)
  }
}
