#' Independent Action
#'
#' @description
#' Calculate independent action response for a set of chemicals with Hill
#' concentration-response curves.
#'
#' @param conc concentrations in regular space
#' @param max maximal (asymptotic) responses
#' @param AC50 concentrations of half-maximal response
#' @param Emax maximum mixture response
#' @param n Hill coefficients (slopes)
#'
#' @return response value
#' @export
#' 
#' @details
#' The concentration is computed as:
#' \deqn{
#'   IA = E_{max} \times
#'   \left(
#'     1 - \prod\limits_{i} \left(1 - \frac{x_i}{E_{max}}\right)
#'   \right),
#' }
#' where \eqn{x_i = hill\_val(conc_i, max_i, AC50_i, n_i)} is the Hill model
#' response function for each chemical.
#' 
#' @seealso \code{\link{hill_val}}
#' 
#' @examples
#' n_chem <- 5
#' conc <- 10^sample(-1:4, n_chem, replace = TRUE)
#' max <- 80 * runif(n_chem)
#' AC50 <- 10^(5 * runif(n_chem) - 1)
#' Emax <- 100
#' 
#' calc_independent_action(conc, max, AC50, Emax)
calc_independent_action <- function(conc, max, AC50, Emax, n = 1) {
  
  # if (any(max > Emax)) {
  #   warning("'max' values are larger than 'Emax'.", call. = FALSE)
  # }
  
  resp <- hill_val(conc, max, AC50, n)
  Emax * (1 - prod((1 - resp / Emax)))
}
