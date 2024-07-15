#' GeoTox S3 object
#' 
#' @description
#' An S3 object that can be used to help organize the data and results of a
#' GeoTox analysis.
#'
#' @param x GeoTox object
#' @param type type of plot
#' @param ... additional arguments passed to plotting functions
#' 
#' @seealso [plot_resp], [plot_hill], [plot_exposure], [plot_sensitivity]
#' @export
GeoTox <- function() {
  structure(
    list(
      par = list(
        n = 1e3,
        IR_params = NULL,
        obesity = list(
          obes_prev  = "OBESITY_CrudePrev",
          obes_sd    = "OBESITY_SD",
          obes_label = "FIPS"
        ),
        exposure = list(
          expos_mean  = "mean",
          expos_sd    = "sd",
          expos_label = "casn"
        ),
        internal_dose = list(
          time    = 1,
          BW      = 1,
          scaling = 1
        ),
        resp = list(
          tp_b_mult = 1.5
        )
      )
    ),
    class = "GeoTox")
}

#' @export
print.GeoTox <- function(x, ...) {
  
  names_simulated <- c("age", "IR", "obesity", "C_ext", "C_ss")
  names_computed <- c("D_int", "C_invitro", "resp")
  names_other <- setdiff(names(x),
                         c(names_simulated, names_computed))
  
  get_info <- function(names) {
    info <- lapply(names, \(name) {
      class <- dim <- ""
      if (is.null(x[[name]])) {
        return(data.frame(Name = name, Class = "", Dim = ""))
      }
      is_list <- inherits(x[[name]], "list")
      if (is_list && length(x[[name]]) > 0) {
        item <- x[[name]][[1]]
      } else if (!is_list) {
        item <- x[[name]]
      } else {
        item <- NULL
      }
      class <- class(item)
      if (any(c("matrix", "data.frame") %in% class)) {
        dim <- paste(dim(item), collapse = " x ")
      } else {
        dim <- length(item)
      }
      if (is_list) {
        dim <- paste0(length(x[[name]]), " x (", dim, ")")
        class <- paste0("list(", class[[1]], ")")
      } else {
        class <- paste(class, collapse = ", ")
      }
      data.frame(Name = name, Class = class, Dim = dim)
    })
    do.call(rbind, info)
  }
  
  info_simulated <- get_info(names_simulated)
  info_simulated <- info_simulated[info_simulated$Class != "", , drop = FALSE]
  info_computed <- get_info(names_computed)
  info_computed <- info_computed[info_computed$Class != "", , drop = FALSE]
  
  cat("GeoTox object\n")
  if (nrow(info_simulated) > 0) {
    n_regions <- length(x[[info_simulated$Name[1]]])
  } else if (nrow(info_computed) > 0) {
    n_regions <- length(x[[info_computed$Name[1]]])
  } else {
    n_regions <- 0
  }
  cat("Regions: ", n_regions, "\n", sep = "")
  cat("Population: ", x$par$n, "\n", sep = "")
  cat("Simulated Fields:")
  if (nrow(info_simulated) > 0) {
    cat("\n")
    print(info_simulated, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Computed Fields:")
  if (nrow(info_computed) > 0) {
    cat("\n")
    print(info_computed, row.names = FALSE, print.gap = 2)
  } else {
    cat(" None\n")
  }
  cat("Other Fields:")
  if (length(names_other) > 0) {
    cat(" ", paste(names_other, collapse = ", "), "\n", sep = "")
  } else {
    cat(" None\n")
  }
}

#' @rdname GeoTox
#' @export
plot.GeoTox <- function(x,
                        type = c("resp", "hill", "exposure", "sensitivity"),
                        ...) {
  type <- match.arg(type)
  switch(type,
         resp = plot_resp(x$resp,
                          region_boundary = x$boundaries$region,
                          group_boundary  = x$boundaries$group,
                          ...),
         hill = plot_hill(x$hill_params),
         exposure = plot_exposure(x$exposure,
                                  region_boundary = x$boundaries$region,
                                  group_boundary  = x$boundaries$group,
                                  ...),
         sensitivity = plot_sensitivity(x, ...))
}
