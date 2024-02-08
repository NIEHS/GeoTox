#' Title
#'
#' @param simulated_css x
#' @param age x
#' @param obesity x
#'
#' @return x
#' @export
sample_Css <- function(simulated_css, age, obesity) {

  if (is.array(age)) age <- list(age)
  if (is.array(obesity)) obesity <- list(obesity)

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
