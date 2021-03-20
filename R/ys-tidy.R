#' Select a subset of columns from a yspec object
#' 
#' @param .x a yspec object
#' @param ... unquoted columns to select
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' ys_select(spec, WT, AGE, ALB)
#' 
#' ys_select(spec, Wt = WT, AGE)
#' 
#' @export
ys_select <- function(.x, ...) {
  keep <- eval_select(expr(c(...)), as.list(.x))
  if(length(keep)==0) {
    return(.x) 
  }
  original <- names(.x)[keep]
  ans <- .x[original]
  names(ans) <- names(keep)
  for(i in seq_along(keep)) {
    ans[[i]][["col"]] <- names(keep)[[i]]
  }
  ans
}

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
#' The intended use is to subset based on variables define in the `dots` list, 
#' however some internal column data is also available to query.
#' 
#' @param x a yspec object
#' @param expr an unquoted expression
#' @param .default a named list of default look up values
#' 
#' @details
#' 
#' The following fields always exist in the spec and are available for 
#' querying in the filter expression:
#' - `col`:  column name `<character>`
#' - `type`: data type `<character>`; either numeric, character, or integer
#' - `discrete`: discrete data flag `<logical>`; yspec sets this to `TRUE`
#'   when the `values` field is populated
#' - `continuous`: continuous dat flag `<logical>`; yspec sets this to `TRUE`
#'   when the `range` field is populated
#' - `short`: the short name `<character>`
#' - `do_lookup`: lookup indicator; yspec sets this to `TRUE` when some or all 
#'   of the column data is defined by an external lookup file
#' 
#' The following fields will be provided defaults when the filter expression 
#' is evaluated: 
#' 
#' - `unit`: as specified by the user `<character>`; default value is  ""
#' - `covariate`: as specified by the user in `dots` `<logical>`; default value
#'   is `FALSE`
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
#' @seealso [ys_rename()], [ys_join()], [ys_select()]
#' @export
ys_filter <- function(x, expr, .default = NULL) {
  assert_that(is_yspec(x))
  def <- list(unit = "", covariate = FALSE, values = "", decode = "")
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


#' Join yspec objects together
#' 
#' @param left a yspec object
#' @param right a yspec object
#' @param ... more yspect objects
#' 
#' @return A single yspec object
#' 
#' @details
#' All inputs must be `yspec` objects. When the `right` spec (or specs passed
#' under `...`) is joined to the `left` spec, columns that exist in both 
#' `left` and `right` are removed from `right` before joining.
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' l <- ys_select(spec, WT, BMI)
#' r <- ys_select(spec, TIME, TAD)
#' rr <- ys_select(spec, EVID, MDV, CMT, BMI)
#' 
#' ys_join(l, r)
#' 
#' ys_join(l, r, rr)
#' 
#' @seealso [ys_rename()], [ys_filter()], [ys_select()]
#' @export
ys_join <- function(left, right, ...) {
  if(missing(right)) return(left)
  assert_that(is_yspec(left))
  assert_that(is_yspec(right))
  take <- setdiff(names(right), names(left))
  if(length(take)==0) return(left)
  right <- ys_select(right, take)
  ans <- c(left, right)
  for(addl in list(...)) {
    ans <- ys_join(ans,addl)
  }
  ans
}

#' Rename spec columns
#' 
#' @param .x a yspec object
#' @param ... tidy rename specification; use `new_name` = `old_name` to rename
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ans <- ys_rename(spec, TIME = TAFD, RENAL = RF)
#' 
#' tail(ans)
#' 
#' @return A yspec object
#' @seealso [ys_join()], [ys_filter()], [ys_select()]
#' @export
ys_rename <- function(.x, ...) {
  assert_that(is_yspec(.x))
  re <- eval_rename(expr(c(...)), as.list(.x))
  names(.x)[re] <- names(re)
  for(i in seq_along(re)) {
    .x[[re[[i]]]][["col"]] <- names(re)[[i]]
  }
  .x
}
