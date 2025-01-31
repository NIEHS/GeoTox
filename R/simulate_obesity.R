#' Simulate obesity status
#'
#' @param x data frame containing obesity data as a percentage from 0 to 100.
#' @param obes_prev column name of prevalence.
#' @param obes_sd column name of standard deviation.
#' @param obes_label column name of labeling term, required if `x` has more than
#' one row.
#' @param n simulated sample size(s).
#'
#' @details
#' The sample size can be either a single value or a vector the same length as
#' the number of rows in x. If a single value is provided, the same
#' sample size is used for all data frames. If a vector is provided, each
#' element corresponds to the sample size for each row in x.
#' 
#' @return List of arrays containing simulated obesity status.
#'
#' @examples
#' # Input has default column names
#' df <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
#'                  OBESITY_SD = c(5, 5, 5),
#'                  FIPS = letters[1:3])
#' simulate_obesity(df, n = 5)
#' # different sample sizes
#' simulate_obesity(df, n = 5:3)
#' 
#' # Input has custom column names
#' df <- data.frame(prev = c(20, 50, 80),
#'                  sd = c(5, 5, 5),
#'                  label = letters[1:3])
#' simulate_obesity(df,
#'                  obes_prev = "prev",
#'                  obes_sd = "sd",
#'                  obes_label = "label",
#'                  n = 5)
#'
#' @export
simulate_obesity <- function(x,
                             obes_prev = "OBESITY_CrudePrev",
                             obes_sd = "OBESITY_SD",
                             obes_label = "FIPS",
                             n = 1e3) {
  
  if (!inherits(x, "data.frame")) {
    stop("`x` must be a data frame")
  }

  if (!all(c(obes_prev, obes_sd) %in% names(x))) {
    stop("`x` must contain columns named by `obes_prev` and `obes_sd`")
  }

  if (nrow(x) > 1 & !(obes_label %in% names(x))) {
    stop("`x` must contain a column named by `obes_label`")
  }

  if (!(length(n) == 1 | length(n) == nrow(x))) {
    stop("`n` must be a single value or a vector the same length as `nrow(x)`")
  }
  
  out <- purrr::pmap(
    list(x[[obes_prev]], x[[obes_sd]], n),
    \(mean, sd, n) {
      p <- stats::rnorm(n, mean, sd) / 100
      p[p < 0] <- 0
      p[p > 1] <- 1
      status <- stats::rbinom(n, 1, p)
      ifelse(status, "Obese", "Normal")
    })

  if (obes_label %in% names(x)) {
    stats::setNames(out, x[[obes_label]])
  } else {
    out
  }
}
