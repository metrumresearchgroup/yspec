#' Get label
#' 
#' Forms a label for a column (`ycol` method) or columns (`yspec` method). Use
#' [purrr::map] or [purrr::map_chr] to simply extract the label field.
#' 
#' @param x ycol or yspec object
#' @param ... passed to label methods
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_get_label(spec)
#' 
#' ys_get_label(spec$WT)
#' \dontrun{
#' purrr:::map(spec,"label")
#' }
#' @seealso [ys_get_short], [ys_get_short], [ys_get_short_unit]
#' @md
#' @export
ys_get_label <- function(x, ...) {
  label(x,...)  
}

#' Get unit
#' 
#' Forms unit for a column (`ycol` method) or columns (`yspec` method). Use
#' [purrr::map] or [purrr::map_chr] to simply extract the unit field.
#' 
#' @param x ycol or yspec object
#' @param parens if `TRUE`, then parens will be added around any unit with one
#' or more character
#' @param default passed to unit
#' @param ... passed to unit methods
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_get_unit(spec)
#' 
#' ys_get_unit(spec$WT)
#' 
#' \dontrun{
#' purrr:::map(spec,"unit")
#' }
#' @seealso [ys_get_short], [ys_get_label], [ys_get_short_unit]
#' @md
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
#' Forms a short for a column (`ycol` method) or columns (`yspec` method). Use
#' [purrr::map] or [purrr::map_chr] to simply extract the short field.
#' 
#' @param x ycol or yspec object
#' @param ... passed to short methods
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_get_short(spec)
#' 
#' ys_get_short(spec$WT)
#' 
#' \dontrun{
#' purrr:::map(spec,"short")
#' }
#' @seealso [ys_get_unit], [ys_get_label], [ys_get_short_unit]
#' @md
#' @export
ys_get_short <- function(x, ...) {
  short(x,...)
}

#' Get short with unit
#' 
#' @param x ycol or yspec object
#' @param ... arguments passed to [ys_get_short] and [ys_get_unit]
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_get_short_unit(spec)
#' 
#' ys_get_short_unit(spec$WT)
#' 
#' @seealso [ys_get_short], [ys_get_unit], [ys_get_label]
#' @md
#' @export
ys_get_short_unit <- function(x, ...) {
  a <- ys_get_short(x, .aslist=FALSE, ...)
  b <- ys_get_unit(x, .aslist=FALSE,...)
  cols <- names(b)
  ans <- trimws(paste(a,b),"right")
  names(ans) <- cols
  ans
}

