#' Simulate obesity
#'
#' @param x data frame containing obesity data as a percentage from 0 to 100.
#' @param prev column name of prevalence.
#' @param sd column name of standard deviation.
#' @param label column name of labeling term, required if `x` has more than one
#' row.
#' @param n simulated sample size.
#'
#' @return Array or named list of arrays containing simulated obesity status.
#'
#' @examples
#' simulate_obesity(geo_tox_data$obesity[1, ], n = 4)
#'
#' simulate_obesity(geo_tox_data$obesity[1:5, ], n = 10)
#'
#' df <- data.frame(label = letters[1:3], prev = c(20, 50, 80), sd = c(5, 5, 5))
#' simulate_obesity(df, label = "label", prev = "prev", sd = "sd", n = 10)
#'
#' @export
simulate_obesity <- function(
    x, prev = "OBESITY_CrudePrev", sd = "OBESITY_SD", label = "FIPS", n = 1e3
) {

  if (!all(c(prev, sd) %in% names(x))) {
    stop("x must contain columns named by 'prev' and 'sd' inputs")
  }

  if (nrow(x) > 1 & !(label %in% names(x))) {
    stop("x must contain a column named by the 'label' input")
  }

  out <- mapply(
    function(mean, sd) {
      p <- stats::rnorm(n, mean, sd) / 100
      p[p < 0] <- 0
      p[p > 1] <- 1
      status <- stats::rbinom(n, 1, p)
      ifelse(status, "Obese", "Normal")
    },
    mean = x[[prev]],
    sd = x[[sd]],
    SIMPLIFY = FALSE
  )

  if (nrow(x) > 1) stats::setNames(out, x[[label]]) else out[[1]]
}
