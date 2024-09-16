#' Simulate obesity status
#'
#' @param x data frame containing obesity data as a percentage from 0 to 100.
#' @param obes_prev column name of prevalence.
#' @param obes_sd column name of standard deviation.
#' @param obes_label column name of labeling term, required if `x` has more than
#' one row.
#' @param n simulated sample size.
#'
#' @return List of arrays containing simulated obesity status.
#'
#' @examples
#' # Input has default column names
#' df <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
#'                  OBESITY_SD = c(5, 5, 5),
#'                  FIPS = letters[1:3])
#' simulate_obesity(df, n = 5)
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

  if (!all(c(obes_prev, obes_sd) %in% names(x))) {
    stop("x must contain columns named by 'obes_prev' and 'obes_sd'")
  }

  if (nrow(x) > 1 & !(obes_label %in% names(x))) {
    stop("x must contain a column named by 'obes_label'")
  }

  out <- mapply(
    function(mean, sd) {
      p <- stats::rnorm(n, mean, sd) / 100
      p[p < 0] <- 0
      p[p > 1] <- 1
      status <- stats::rbinom(n, 1, p)
      ifelse(status, "Obese", "Normal")
    },
    mean = x[[obes_prev]],
    sd = x[[obes_sd]],
    SIMPLIFY = FALSE
  )

  if (obes_label %in% names(x)) {
    stats::setNames(out, x[[obes_label]])
  } else {
    out
  }
}
