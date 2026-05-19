#' Independent action
#'
#' Calculate independent action response for a set of chemicals with Hill
#' concentration-response curves.
#'
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
#' @param conc Concentrations in regular space.
#' @param max Maximal (asymptotic) responses.
#' @param AC50 Concentrations of half-maximal response.
#' @param Emax Maximum mixture response.
#' @param n Hill coefficients (slopes).
#'
#' @returns Response value.
#' @noRd
calc_independent_action <- function(conc, max, AC50, Emax, n = 1) {
  resp <- hill_val(conc, max, AC50, n)
  Emax * (1 - prod((1 - resp / Emax)))
}
