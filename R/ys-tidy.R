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
#' @md
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

ys_filter_impl <- function(x, expr, def, import, chk_vars, enclos) {
  env <- x[unique(c(import, names(def)))]
  if(is.list(x[["dots"]])) {
    env <- combine_list(env, x[["dots"]])
  }
  env <- combine_list(def,env)
  if(!all(chk_vars %in% names(x[["dots"]]))) return(FALSE)
  isTRUE(eval(expr, envir = env, enclos = enclos))
}

#' Subset spec items using column values
#' 
#' The intended use is to subset based on variables define in the `dots` list, 
#' however some internal column data is also available to query.
#' 
#' @param x a yspec object
#' @param expr an unquoted expression
#' @param .default a named list or environment containing defaults for `expr`; 
#' consider using [ys_fill_dots()] as an alternative to passing `.default`
#' @param .enclos an enclosing environment for evaluatingn `expr`
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
#' @section Evaluation environment: 
#' In order to determine if any column should get selected, `ys_filter()`, 
#' builds an environment and evaluates `expr` in that environment. Columns are 
#' selected only if `expr` evalutes to `TRUE` (via [isTRUE()]). 
#' 
#' The environment is comprised of pre-existing data items in the spec (e.g. 
#' `col` or `short`; these items are always present), data items in the enclosing
#' environment (`.enclos`; this defaults to `parent.frame()`), the `.defaults` 
#' list (passed by the user at run time) and the `.dots` list associated with 
#' each column. 
#' 
#' Users are encouraged to filter based on logical data items in `dots` that are
#' set through the `flags` field in `SETUP__`. When `flags` are set, every 
#' column is given a logical data item that can always be evaluated for every 
#' column. This is the safest and simplest way to go and should be the target
#' usage. In case more complicated applications are required, users can appeal
#' to data items in `dots` which may or may not be logical. Of course, the user
#' can enter data into `dots` for every column to ensure that data item is 
#' available for evaluating `expr` but that might not be very convenient. In 
#' that case, pass a list of `.defaults` that will be used when the filter
#' variable isn't available in `dots`. As an alternative to using `.defaults`, 
#' the user can run the `spec` object through `ys_dots_fill` which will 
#' fill in dots data items with default values only when they don't exist. This
#' is probably more convenient, but the user is warned that these data items 
#' in `dots` stay with the `yspec` object for the life of the object. 
#' 
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
#' @seealso [ys_rename()], [ys_join()], [ys_select()], [ys_fill_dots()]
#' @md
#' @export
ys_filter <- function(x, expr, .default = NULL, .enclos = parent.frame()) {
  assert_that(is_yspec(x))
  def <- list(unit = "", covariate = FALSE, values = "", decode = "")
  if(is.list(.default) || is.environment(.default)) {
    .default <- as.list(.default)
    assert_that(is_named(.default))
    def <- combine_list(def, .default)    
  }
  expr <- quo_get_expr(enquo(expr))
  expr_vars <- all.vars(expr)
  # take these from the spec; they are always present
  import <- c("col", "type", "discrete", "short", "do_lookup", "continuous")
  # Develop a vector of variables that we'll need to find from dots
  # Starting with vars in expression, drop those from import or defaults
  chk_vars <- setdiff(expr_vars, c(import, names(def)))
  # Now, drop those that are available from enclose
  if(length(chk_vars)  > 0) {
    chk_vars <- setdiff(chk_vars, names(.enclos))
  }
  # now, if we don't find chk_vars in the column level dots, we'll the column
  # will not be selected
  ans <- map_lgl(
    x, 
    ys_filter_impl, 
    expr = expr, 
    def = def, 
    import = import, 
    chk_vars = chk_vars, 
    enclos = .enclos
  )
  if(sum(ans)==0) {
    warning("no columns were selected when filtering", call. = FALSE)  
  }
  x[ans]
}

#' Supplement data in the dots list with default values
#' 
#' Please avoid thinking of this function as a way to mutate `dots`.  It sort of
#' is but not in the way you think. It really is for supplementing the spec
#' so that downstream functions are guaranteed to find these names with 
#' sensible values for every column in the spec. mutate will be a different 
#' function.
#' 
#' @param x a yspec object
#' @param ... `name = value` data to be added to `dots`
#' @param .overwrite logical indicating whether `x` should be overwritten with 
#' data in `...`
#' 
#' @details
#' By default, this function will not overwrite data in `dots` when it already 
#' exists. This behavior can be changed with the `.overwrite` argument. Note
#' that when `.overwrite` is `TRUE`, all columns in the cpec will have the 
#' same value in `dots` for the `name` getting set.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' spec2 <- ys_fill_dots(spec, new_var = FALSE)
#' 
#' @md
#' @export
ys_fill_dots <- function(x, ..., .overwrite = FALSE) {
  assert_that(is_yspec(x))
  .defaults <- list(...)
  modify(x, ys_fill_dots_impl, .defaults = .defaults, .overwrite = .overwrite) 
}

#' @param x a ycol object
#' @param .defaults a named list of default values for `dots`
#' @param .overwrite logical indicating whether x should be overwritten or not
#' @noRd
ys_fill_dots_impl <- function(x, .defaults, .overwrite) {
  assert_that(is_named(.defaults), msg = "defaults for `dots` must be named")
  if(!is.list(x[["dots"]])) x[["dots"]] <- list()
  if(.overwrite) {
    x[["dots"]] <- ys_update_list(x[["dots"]], .defaults)  
  } else {
    x[["dots"]] <- combine_list(.defaults, x[["dots"]])  
  }
  x
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
#' @md
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
#' @md
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