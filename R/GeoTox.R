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
  structure(list(), names = character(0), class = "GeoTox")
}

#' @export
print.GeoTox <- function(x, ...) {
  
  default_names <- c("age", "IR", "obesity", "C_ext", "C_ss",
                     "D_int", "C_invitro", "resp")
  other_names <- setdiff(names(x), default_names)
  
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
  
  info <- get_info(default_names)
  info <- info[info$Class != "", , drop = FALSE]
  
  cat("GeoTox object\n")
  if (nrow(info) > 0) {
    cat("Regions: ", length(x[[info$Name[1]]]), "\n", sep = "")
    cat("Population: ", x$inputs$n, "\n", sep = "")
    cat("Fields:\n")
    print(info, row.names = FALSE, print.gap = 2)
  }
  # if (length(other_names) > 0) {
  #   cat("Other:\n  ")
  #   cat(paste(other_names, collapse = ", "))
  #   cat("\n")
  # }
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
         hill = plot_hill(x$inputs$hill_params),
         exposure = plot_exposure(x$inputs$exposure$x,
                                  region_boundary = x$boundaries$region,
                                  group_boundary  = x$boundaries$group,
                                  ...),
         sensitivity = plot_sensitivity(x, ...))
}
