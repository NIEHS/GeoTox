#' Sample from pre-generated Css data
#'
#' @param simulated_css list of pre-generated Css data, for details see:
#' \code{vignette("package_data", package = "GeoTox")}.
#' @param age list or atomic vector of ages.
#' @param obesity list or atomic vector of obesity status.
#'
#' @return list of matrices containing Css values. Columns are sorted to have
#' consistent order across functions.
#' 
#' @examples
#' # Vector inputs
#' sample_Css(geo_tox_data$simulated_css,
#'            c(15, 25, 35),
#'            c("Normal", "Obese", "Normal"))
#' 
#' # List inputs
#' sample_Css(geo_tox_data$simulated_css,
#'            list(c(34, 29), 55),
#'            list(c("Obese", "Normal"), "Normal"))
#' @export
sample_Css <- function(simulated_css, age, obesity) {

  if (!is.list(age)) age <- list(age)
  if (!is.list(obesity)) obesity <- list(obesity)

  mapply(
    function(age, obesity) {
      out <- matrix(NA, nrow = length(age), ncol = length(simulated_css))
      for (i in 1:length(simulated_css)) {
        df <- simulated_css[[i]]
        row_idx <- mapply(
          function(age, obesity) {
            idx <- utils::tail(which(df$age_min <= age & df$weight == obesity), 1)
            ifelse(length(idx) == 1, idx, NA)
          },
          age,
          obesity
        )
        for (j in sort(unique(row_idx))) {
          idx <- which(row_idx == j)
          out[idx, i] <- sample(df$css[[j]], length(idx), replace = TRUE)
        }
      }
      colnames(out) <- names(simulated_css)
      # Have consistent output order
      out[, order(colnames(out)), drop = FALSE]
    },
    age,
    obesity,
    SIMPLIFY = FALSE
  )
}
