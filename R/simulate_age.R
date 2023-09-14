#' Simulate age data
#'
#' @param x data frame containing population data for age groups.
#' @param n simulated sample size.
#' @param return_mean return mean age.
#'
#' @return array of simulated ages.
#' @export
#'
#' @examples
#' x <- data.frame(AGEGRP = 0:18, TOT_POP = c(sum(1:18), 1:18))
#' simulate_age(x, 10)
#' simulate_age(x, 10, TRUE)
simulate_age <- function(x, n = 1e3, return_mean = FALSE) {

  # Check columns
  if (!all(c("AGEGRP", "TOT_POP") %in% names(x))) {
    stop("x must contain columns 'AGEGRP' and 'TOT_POP'")
  }

  # Enforce order and check values of age group
  x <- x[order(x$AGEGRP), ]
  if (!(nrow(x) == 19 && all(x$AGEGRP == 0:18))) {
    stop("x$AGEGRP must contain values 0:18")
  }

  # Probability of each age group
  prob <- x$TOT_POP[-1] / x$TOT_POP[1]

  # Assume equal probability within group
  prob <- rep(prob / 5, each = 5)

  # Sample ages
  ages <- sample(0:89, size = n, prob = prob, replace = TRUE)

  # Return
  if (return_mean) {
    rep(round(mean(ages)), n)
  } else {
    ages
  }
}
