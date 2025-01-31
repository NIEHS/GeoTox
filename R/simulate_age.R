#' Simulate ages
#'
#' @param x data frame or list of data frames containing population data for age
#' groups. Each data frame must contain columns "AGEGRP" and "TOT_POP".
#' @param n simulated sample size(s).
#'
#' @details
#' Each data frame must contain 19 rows. The first row represents the total
#' population of all age groups while the next 18 rows represent age groups
#' from 0 to 89 in increments of 5 years.
#' 
#' The sample size can be either a single value or a vector the same length as
#' the number of data frames in x. If a single value is provided, the same
#' sample size is used for all data frames. If a vector is provided, each
#' element corresponds to the sample size for each data frame in x.
#' 
#' @return List of arrays containing simulated ages.
#'
#' @examples
#' # Single data frame
#' x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
#' # populate only age range 40-44, set population total of all ages
#' x$TOT_POP[c(1, 10)] <- 100
#' simulate_age(x, 5)
#' 
#' # List of 2 data frames
#' y <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
#' # populate age ranges 5-9 and 50-54
#' y$TOT_POP[c(3, 12)] <- 10
#' # set population total for all age groups
#' y$TOT_POP[1] <- sum(y$TOT_POP)
#' simulate_age(list(x = x, y = y), 15)
#' # different sample sizes
#' simulate_age(list(x = x, y = y), c(15, 10))
#'
#' @export
simulate_age <- function(x, n = 1e3) {

  x <- .check_types(x,
                    "data.frame",
                    "`x` must be a data frame or list of data frames")

  if (.check_names(x, c("AGEGRP", "TOT_POP"))) {
    stop("`x` data frames must contain columns 'AGEGRP' and 'TOT_POP'")
  }
  
  if (any(unlist(lapply(x, \(y) nrow(y) != 19)))) {
    stop("`x` data frames must contain 19 rows")
  }
  
  if (!(length(n) == 1 | length(n) == length(x))) {
    stop("`n` must be a single value or a vector with values for ",
         "each data frame in `x`")
  }
  
  purrr::pmap(list(x, n), \(df, n) .simulate_age(df, n))
  
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
