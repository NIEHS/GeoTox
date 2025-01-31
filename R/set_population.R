#' Set population data
#'
#' @param x GeoTox object.
#' @param age numeric vector or list of numeric vectors of age values.
#' @param obesity character vector or list of character vectors of obesity
#' status.
#'
#' @return The same object with simulated fields added.
#' @export
#'
#' @examples
#' # Single region
#' age <- round(runif(10, 1, 100))
#' obesity <- sample(c("Normal", "Obese"), 10, replace = TRUE)
#' geoTox <- set_population(GeoTox(), age = age, obesity = obesity)
#' 
#' # Multiple regions
#' age <- list("37001" = round(runif(10, 1, 100)),
#'             "37007" = round(runif(8, 1, 100)))
#' obesity <- list("37001" = sample(c("Normal", "Obese"), 10, replace = TRUE),
#'                 "37007" = sample(c("Normal", "Obese"), 8, replace = TRUE))
#' geoTox <- set_population(GeoTox(), age = age, obesity = obesity)
set_population <- function(x, age = NULL, obesity = NULL) {
  
  set_age <- !is.null(age)
  set_obesity <- !is.null(obesity)
  
  if (set_age) {
    age <- .check_types(age,
                        c("numeric", "integer"),
                        paste0("`age` must be a numeric vector or list of ",
                               "numeric vectors"))
  }
  
  if (set_obesity) {
    obesity <- .check_types(obesity,
                            "character",
                            paste0("`obesity` must be a character vector or ",
                                   "list of character vectors"))
    if (any(purrr::map_lgl(obesity,
                           \(x) !all(x %in% c("Normal", "Obese"))))) {
      stop("`obesity` values must be 'Normal' or 'Obese'", call. = FALSE)
    }
  }
  
  # Update population size
  if (set_age) n_age <- purrr::map_int(age, length)
  if (set_obesity) n_obesity <- purrr::map_int(obesity, length)
  if (set_age & set_obesity) {
    if (!identical(n_age, n_obesity)) {
      stop("Population sizes for `age` and `obesity` do not match",
           call. = FALSE)
    }
    x$par$n <- n_age
  } else if (set_age) {
    x$par$n <- n_age
  } else if (set_obesity) {
    x$par$n <- n_obesity
  }
  
  # Set fields
  if (set_age) x$age <- age
  if (set_obesity) x$obesity <- obesity
  
  # Clear downstream fields
  if (set_age & !is.null(x$IR)) {
    warning("Clearing `IR` field", call. = FALSE)
    x$IR <- NULL
  }
  if ((set_age | set_obesity) &
      !(is.null(x$C_ss) & is.null(x$css_sensitivity))) {
    warning("Clearing `C_ss` and `css_sensitivity` fields", call. = FALSE)
    x$C_ss <- NULL
    x$css_sensitivity <- NULL
  }
  
  x
}
