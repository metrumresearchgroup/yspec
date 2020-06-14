#' Get label
#' 
#' Forms a label for a column (`ycol` method) or columns (`yspec` method)
#' 
#' @param x ycol or yspec object
#' @param ... passed to label methods
#' @seealso [ys_get_short], [ys_get_short], [ys_get_short_unit]
#' @export
ys_get_label <- function(x, ...) {
  label(x,...)  
}

#' Get unit
#' 
#' Forms unit for a column (`ycol` method) or columns (`yspec` method)
#' 
#' @param x ycol or yspec object
#' @param parens if `TRUE`, then parens will be added around any unit with one
#' or more character
#' @param default passed to [yspec:::unit]
#' @param ... passed to unit methods
#' @seealso [ys_get_short], [ys_get_label], [ys_get_short_unit]
#' @export
ys_get_unit <- function(x, parens = FALSE, default = "",...) {
  ans <- unit(x, default = default, ...)
  if(parens) {
    add_parens <- sapply(ans,nchar) > 0
    if(any(add_parens)) {
      ans[add_parens] <- paste0("(",ans[add_parens],")")
    }
  }
  ans
}

#' Get short
#' 
#' Forms a short for a column (`ycol` method) or columns (`yspec` method)
#' 
#' @param x ycol or yspec object
#' @param ... passed to label methods
#' @seealso [ys_get_unit], [ys_get_label], [ys_get_short_unit]
#' @export
ys_get_short <- function(x, ...) {
  short(x,...)
}

#' Get short with unit
#' 
#' @param x ycol or yspec object
#' @param ... arguments passed to [ys_get_short] and [ys_get_unit]
#' @seealso [ys_get_short], [ys_get_unit], [ys_get_label]
#' @export
ys_get_short_unit <- function(x, ...) {
  a <- ys_get_short(x, .aslist=FALSE, ...)
  b <- ys_get_unit(x, .aslist=FALSE,...)
  cols <- names(b)
  ans <- trimws(paste(a,b),"right")
  names(ans) <- cols
  ans
}

