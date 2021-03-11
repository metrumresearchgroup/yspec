ys_filter_impl <- function(x, expr, def) {
  if(is.list(x[["dots"]])) {
    x <- combine_list(x,x[["dots"]])
  }
  x <- combine_list(def, x)
  isTRUE(eval(expr, envir = x, enclos = baseenv()))
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
ys_filter <- function(x, expr, .default = NULL) {
  assert_that(is_yspec(x))
  def <- list(
    unit = "", lookup = FALSE, values = "", 
    decode = "", covariate = FALSE
  )
  if(is.list(.default)) {
    assert_that(rlang::is_named(.default))
    def <- combine_list(def, .default)    
  }
  expr <- quo_get_expr(enquo(expr))
  ans <- map_lgl(x, ys_filter_impl, expr = expr, def = def)
  if(sum(ans)==0) {
    warning("no columns were selected when filtering", call. = FALSE)  
  }
  x[ans]
}
