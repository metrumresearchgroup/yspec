ys_filter_impl <- function(x, ex, def) {
  if(is.list(x[["dots"]])) {
    x <- combine_list(x[["dots"]], x)
  }
  x <- combine_list(def, x)
  isTRUE(eval(ex, x, enclos = baseenv()))
}

#' Select columns from spec object
#' 
#' @param spec a yspec object
#' @param expr an unquoted expression
#' @param .default a named list of default look up values
#' 
#' @return 
#' A `yspec` object.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_filter(spec, is.character(decode))
#' 
#' ys_filter(spec, unit == "kg" | type == "character")
#' 
#' @export
ys_filter <- function(spec, expr, .default = NULL) {
  assert_that(is_yspec(spec))
  def <- list(
    unit = '<null>', lookup = FALSE, values = "<null>", 
    decode = "<null>", covariate = FALSE
  )
  if(is.list(.default)) {
    assert_that(rlang::is_named(.default))
    def <- combine_list(def, .default)    
  }
  expr <- quo_get_expr(enquo(expr))
  ans <- map_lgl(spec, ys_filter_impl, ex = expr, def = def)
  spec[ans]
}
