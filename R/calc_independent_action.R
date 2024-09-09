#' Independent Action
#'
#' @description
#' Calculate independent action response for a set of chemicals with Hill
#' concentration-response curves.
#'
#' @param conc concentrations in regular space
#' @param max maximal (asymtotic) responses
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
#'   IA = E_{max}
#'   \left(
#'     1 - \prod\limits_{i} \left(1 - \frac{x_i}{E_{max}}\right)
#'   \right),
#' }
#' where \eqn{x_i = hill\_val(conc_i, max_i, AC50_i, n_i)} is the Hill model
#' response function for each chemical.
#' 
#' @seealso \code{\link{hill_val}}
calc_independent_action <- function(conc, max, AC50, Emax, n = 1) {
  
  # if (any(max > Emax)) {
  #   warning("'max' values are larger than 'Emax'.", call. = FALSE)
  # }
  
  resp <- hill_val(conc, max, AC50, n)
  Emax * (1 - prod((1 - resp / Emax)))
}
