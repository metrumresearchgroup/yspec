

##' Generate axis labels or col/label structures for plots
##' 
##' @param .spec a yspec object
##' @param ... unquoted column names in the spec
##' @param .fun a function that forms the axis label data for 
##' @param x a ycol object
##' @param .add_unit if \code{TRUE}, the unit is appended to the axis title
##' if it is found
##' a single column
##' 
##' @export
axis_labs <- function(.spec, ..., .fun = axis_label) {
  vars <- select_vars(names(.spec), !!!quos(...))
  map_chr(.spec[vars], .fun)
}

##' @rdname axis_labs
##' @export
axis_col_labs <- function(.spec, ..., .fun = axis_label) {
  ans <- axis_labs(.spec,..., .fun = .fun)  
  set_names(paste0(names(ans), "//", ans), names(ans))
}

##' @rdname axis_labs
##' @export
axis_label <- function(x, .add_unit = TRUE) {
  label <- x[["axis"]]
  if(is.null(label)) {
    label <- x[["short"]]  
  }
  if(!.add_unit) return(label)
  unit <- x[["unit"]]
  if(!is.null(unit)) {
    unit <- paste0(" (",unit,")")  
  }
  paste0(label, unit)
}

##' @rdname axis_labs
##' @export
axis_asis <- function(x) {
  axis_label(x, .add_unit = FALSE)  
}

