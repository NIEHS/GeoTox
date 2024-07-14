#' Simulate external exposure
#'
#' @param x data frame or list of data frames containing exposure data.
#' @param expos_mean column name of mean values.
#' @param expos_sd column name of standard deviations.
#' @param expos_label column name of labeling term, required if `x` has more
#' than one row.
#' @param n simulated sample size.
#'
#' @return list of matrices containing inhalation rates. Matrix columns are
#' named using the values in the `expos_label` column for more than one data
#' frame row. Columns are sorted to have consistent order across functions.
#'
#' @examples
#' # Single data frame
#' x <- data.frame(mean = 1:3, sd = (1:3) / 10, casn = letters[1:3])
#' simulate_exposure(x, n = 5)
#' 
#' # List of 2 data frames
#' y <- data.frame(mean = 4:6, sd = 0.1, casn = letters[1:3])
#' simulate_exposure(list(loc1 = x, loc2 = y), n = 5)
#' 
#' # Input has custom column names
#' z <- data.frame(ave = 1:3, stdev = (1:3) / 10, chem = letters[1:3])
#' simulate_exposure(z,
#'                   expos_mean = "ave",
#'                   expos_sd = "stdev",
#'                   expos_label = "chem",
#'                   n = 5)
#'
#' @export
simulate_exposure <- function(x,
                              expos_mean = "mean",
                              expos_sd = "sd",
                              expos_label = "casn",
                              n = 1e3) {

  if (!any(c("data.frame", "list") %in% class(x))) {
    stop("x must be a data.frame or list")
  }
  
  if (is.data.frame(x)) x <- list(x)

  if (.check_names(x, c(expos_mean, expos_sd))) {
    stop("x data frames must contain columns named by 'expos_mean' and ",
         "'expos_sd'")
  }
  
  lapply(x, function(df) {
    out <- .simulate_exposure(df, expos_mean, expos_sd, n)
    if (expos_label %in% names(df)) {
      colnames(out) <- df[[expos_label]]
      # Have consistent output order
      out <- out[, order(colnames(out)), drop = FALSE]
    } else if (nrow(df) > 1) {
      stop("x data frames must contain a column named by 'expos_label'",
           call. = FALSE)
    }
    out
  })
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
