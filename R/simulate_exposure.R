#' Simulate external exposure
#'
#' @param x data frame or list of data frames.
#' @param mean column name of mean values.
#' @param sd column name of standard deviations.
#' @param label column name of labeling term, required if `x` has more than one
#' row.
#' @param n simulated sample size.
#'
#' @return A matrix or list of matrices containing inhalation rates. Matrix
#' columns are named using the values in the `label` column for more than one
#' data frame row.
#'
#' @examples
#' data <- split(geo_tox_data$exposure, ~FIPS)
#'
#' # Single data frame
#' simulate_exposure(data[[1]], n = 5)
#'
#' # List of data frames
#' simulate_exposure(data[1:3], n = 5)
#'
#' @export
simulate_exposure <- function(
    x, mean = "mean", sd = "sd", label = "casn", n = 1e3
) {

  if (!any(methods::is(x, "data.frame"), methods::is(x, "list"))) {
    stop("x must be a data.frame or list")
  }

  if (methods::is(x, "data.frame")) {

    if (!all(c(mean, sd) %in% names(x))) {
      stop("x must contain columns named by 'mean' and 'sd' inputs")
    }

    if (nrow(x) > 1 & !(label %in% names(x))) {
      stop("x must contain a column named by the 'label' input")
    }

    out <- .simulate_exposure(x, mean, sd, n)
    if (nrow(x) > 1) {
      colnames(out) <- x[[label]]
      # Have consistent output order
      out <- out[order(colnames(out))]
    }
    out

  } else {

    if (!all(sapply(x, function(df) all(c(mean, sd) %in% names(df))))) {
      stop("x data frames must contain columns named by 'mean' and 'sd' inputs")
    }

    nrow <- sapply(x, nrow)
    label_exists <- sapply(x, function(df) label %in% names(df))

    if (!all(nrow > 1 & label_exists)) {
      stop("x data frames must contain a column named by the 'label' input")
    }

    lapply(x, function(df) {
      out <- .simulate_exposure(df, mean, sd, n)
      if (nrow(df) > 1) {
        colnames(out) <- df[[label]]
        # Have consistent output order
        out <- out[, order(colnames(out)), drop = FALSE]
      }
      out
    })

  }
}

.simulate_exposure <- function(x, mean, sd, n) {

  mean <- x[[mean]]
  sd <- x[[sd]]

  if (length(mean) == 0) {
    matrix(0, nrow = n, ncol = 0)
  } else {
    mapply(
      function(mean, sd, n) {
        if (mean == 0) {
          rep(0, n)
        } else if (mean > 0 & is.na(sd)) {
          rep(mean, n)
        } else {
          truncnorm::rtruncnorm(
            n, a = 0, b = Inf, mean = mean, sd = sd
          )
        }
      },
      mean = mean,
      sd = sd,
      n = n
    )
  }
}
