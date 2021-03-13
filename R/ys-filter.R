ys_filter_impl <- function(x, expr, def, import, chk_vars) {
  env <- x[unique(c(import, names(def)))]
  if(is.list(x[["dots"]])) {
    env <- combine_list(env, x[["dots"]])
  }
  env <- combine_list(def,env)
  if(!all(chk_vars %in% names(env))) return(FALSE)
  isTRUE(eval(expr, envir = env, enclos = baseenv()))
}

#' Subset spec items using column values
#' 
#' @param x a yspec object
#' @param expr an unquoted expression
#' @param .default a named list of default look up values
#' 
#' @details
#' 
#' The following fields always exist in the spec and are available for 
#' querying in the filter expression:
#' - `col`
#' - `type`
#' - `discrete`
#' - `continuous`
#' - `short`
#' - `do_lookup`
#' 
#' The following fields will be provided defaults when the filter expression 
#' is evaluated: 
#' 
#' - `unit` = ""
#' - `values` = ""
#' - `decode` = ""
#' - `covariate` = `FALSE`
#' 
#' In addition to these fields, you can build the filter expression using 
#' items in the `dots` field. 
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
    unit = "",  values = "", decode = "", covariate = FALSE
  )
  if(is.list(.default)) {
    assert_that(is_named(.default))
    def <- combine_list(def, .default)    
  }
  expr <- quo_get_expr(enquo(expr))
  # variables in the test expression
  all_vars <- all.vars(expr)
  # take these from the spec; they are always present
  import <- c("col", "type", "discrete", "short", "do_lookup", "continuous")
  # if we can't find the chk_vars, the test will be false
  chk_vars <- setdiff(all_vars, c(import, names(def)))
  ans <- map_lgl(
    x, 
    ys_filter_impl, 
    expr = expr, 
    def = def, 
    import = import, 
    chk_vars = chk_vars
  )
  if(sum(ans)==0) {
    warning("no columns were selected when filtering", call. = FALSE)  
  }
  x[ans]
}
