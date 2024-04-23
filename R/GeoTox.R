#' GeoTox S3 object
#' 
#' @description
#' An S3 object that can be used to help organize the data and results
#' of a GeoTox analysis. There are several methods that can be used
#' on a GeoTox object: [simulate], [calc] and the plot generic S3 method
#' detailed below.
#'
#' @param x a GeoTox object
#' @param type type of plot
#' @param ... additional arguments passed to plotting functions
#' 
#' @seealso [simulate], [calc]
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
  
  info <- get_info(c(default_names, other_names))
  info <- info[info$Class != "", , drop = FALSE]
  
  cat("GeoTox object\n")
  if (nrow(info) > 0) {
    cat("Regions: ", length(x[[info$Name[1]]]), "\n", sep = "")
    cat("Fields:\n")
    print(info, row.names = FALSE, print.gap = 2)
  }
}

#' @rdname GeoTox
#' @export
plot.GeoTox <- function(x, type = "resp", ...) {
  type <- match.arg(type)
  switch(type,
         resp = plot_resp(x, ...),
         stop("Invalid plot type"))
}
