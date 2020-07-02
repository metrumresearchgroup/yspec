
#' Generate axis labels or col/label structures for plots
#'
#' @param .spec a yspec object
#' @param vars a quosure or character vector; passed to 
#' [tidyselect::vars_select]
#' @param ... arguments passed to [short]
#' @param .fun a function that forms the axis label data for 
#' @param x a ycol object
#' @param .add_unit if `TRUE`, the unit is appended to the axis title
#'  if it is found
#'
#' @md
#' @export
axis_labs <- function(.spec, vars=NULL, .fun = axis_label,...) {
  if(missing(vars)) vars <- names(spec)
  if(is.character(vars)) {
    vars <- cvec_cs(vars) 
  } 
  vars <- vars_select(names(.spec), !!!vars)
  ans <- map_chr(.spec[vars], .fun,...)
  ans
}

#' @rdname axis_labs
#' @export
axis_col_labs <- function(.spec, ..., .fun = axis_label) {
  ans <- axis_labs(.spec,..., .fun = .fun)  
  set_names(paste0(names(ans), "//", ans), names(ans))
}

#' @rdname axis_labs
#' @export
axis_label <- function(x, .add_unit = TRUE,...) {
  label <- x[["axis"]]
  if(is.null(label)) {
    label <- short(x,...)
  }
  if(!.add_unit) return(label)
  unit <- ys_get_unit(x, parens = TRUE)
  trimws(paste(label, unit))
}

#' @rdname axis_labs
#' @export
axis_asis <- function(x) {
  axis_label(x, .add_unit = FALSE)  
}

