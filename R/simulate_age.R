#' Simulate ages
#'
#' @param x data frame or list of data frames containing population data for age
#' groups. Each data frame must contain columns "AGEGRP" and "TOT_POP".
#' @param n simulated sample size.
#'
#' @return List of arrays containing simulated ages.
#'
#' @examples
#' # Single data frame
#' data <- geo_tox_data$age[geo_tox_data$age$FIPS == 37001, ]
#' ages <- simulate_age(data, 10)
#'
#' # List of data frames
#' data <- split(geo_tox_data$age, ~FIPS)
#' ages <- simulate_age(data, 10)
#'
#' @export
simulate_age <- function(x, n = 1e3) {

  if (!any(c("data.frame", "list") %in% class(x))) {
    stop("x must be a data.frame or list")
  }

  if (is.data.frame(x)) x <- list(x)

  if (.check_names(x, c("AGEGRP", "TOT_POP"))) {
    stop("x data frames must contain columns 'AGEGRP' and 'TOT_POP'")
  }
  
  lapply(x, function(df) .simulate_age(df, n))
  
}

.simulate_age <- function(x, n) {

  # Ensure order for AGEGRP
  x <- x[order(x$AGEGRP), ]

  # Probability of each age group
  prob <- x$TOT_POP[-1] / x$TOT_POP[1]

  # Assume equal probability within group
  prob <- rep(prob / 5, each = 5)

  # Sample ages
  sample(0:89, size = n, prob = prob, replace = TRUE)
}
