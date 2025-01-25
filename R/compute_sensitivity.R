#' Compute response sensitivity to parameter variation.
#'
#' @param x GeoTox object.
#' @param vary which parameter to vary.
#' @param max_mult input for [calc_concentration_response] step.
#'
#' @return output from [calc_concentration_response]
#' @export
#' 
#' @examples
#' # Use a subset of the package data for demonstration purposes
#' set.seed(2357)
#' n <- 10 # Population size
#' m <- 5 # Number of regions
#' idx <- if (m < 100) sample(1:100, m) else 1:100
#'
#' # Create GeoTox object and populate required fields
#' geoTox <- GeoTox() |>
#'   # Simulate populations for each region
#'   simulate_population(age = split(geo_tox_data$age, ~FIPS)[idx],
#'                       obesity = geo_tox_data$obesity[idx, ],
#'                       exposure = split(geo_tox_data$exposure, ~FIPS)[idx],
#'                       simulated_css = geo_tox_data$simulated_css,
#'                       n = n) |>
#'   # Estimated Hill parameters
#'   set_hill_params(geo_tox_data$dose_response |>
#'                     fit_hill(assay = "endp", chem = "casn") |>
#'                     dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed))
#'
#' # Sensitivity computations can now be done
#' age_resp <- geoTox |> compute_sensitivity()
#' obesity_resp <- geoTox |> compute_sensitivity(vary = "obesity")
compute_sensitivity <- function(x,
                                vary = c("age", "obesity", "css_params",
                                         "fit_params", "C_ext"),
                                max_mult = NULL) {
  
  vary <- match.arg(vary)
  
  C_ss <- switch(vary,
                 age         = x$css_sensitivity$age,
                 obesity     = x$css_sensitivity$obesity,
                 css_params  = x$css_sensitivity$params,
                 x$css_sensitivity$other)
  
  if (is.null(max_mult)) {
    max_mult <- x$par$resp$max_mult
  }
  
  if (is.null(x$age)) {
    stop("GeoTox 'age' field is not set.", call. = FALSE)
  }
  
  if (vary == "age") {
    age <- x$age
    IR <- x$IR
    if (is.null(IR)) {
      stop("GeoTox 'IR' field is not set.", call. = FALSE)
    }
  } else {
    age <- lapply(x$age, function(x) rep(stats::median(x),
                                         length.out = length(x)))
    IR <- simulate_inhalation_rate(age, IR_params = x$par$IR_params)
  }
  
  if (vary == "C_ext") {
    C_ext <- x$C_ext
    if (is.null(C_ext)) {
      stop("GeoTox 'C_ext' field is not set.", call. = FALSE)
    }
  } else {
    # Set exposure sd = NA (or 0)
    exposure <- lapply(x$exposure, \(x) x |> dplyr::mutate(sd = NA))
    C_ext <- simulate_exposure(x           = exposure,
                               expos_mean  = x$par$exposure$expos_mean,
                               expos_sd    = x$par$exposure$expos_sd,
                               expos_label = x$par$exposure$expos_label,
                               n           = x$par$n)
  }
  
  D_int <- calc_internal_dose(C_ext,
                              IR,
                              time    = x$par$internal_dose$time,
                              BW      = x$par$internal_dose$BW,
                              scaling = x$par$internal_dose$scaling)
  C_invitro <- calc_invitro_concentration(D_int, C_ss)
  resp <- calc_concentration_response(C_invitro,
                                      x$hill_params,
                                      max_mult = max_mult,
                                      fixed     = vary != "fit_params")
  
  resp
}
