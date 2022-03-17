#' Select a subset of items from a yspec object
#' 
#' @param .x A yspec object.
#' @param ... Unquoted columns to select.
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' ys_select(spec, WT, AGE, ALB)
#' 
#' ys_select(spec, Wt = WT, AGE)
#'  
#' length(ys_select(spec))
#'  
#' @details
#' If no columns are selected, then an empty `yspec` object is returned. 
#' 
#' @return 
#' A `yspec` object that may be length zero if no columns were selected. 
#' 
#' @md
#' @export
ys_select <- function(.x, ...) {
  assert_that(is_yspec(.x))
  keep <- eval_select(expr(c(...)), as.list(.x))
  if(length(keep)==0) {
    return(.x[NULL]) 
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

#' Subset yspec items using column values
#' 
#' The intended use is to subset based on variables define in the `dots` list, 
#' however some internal column data is also available to query.
#' 
#' @param x A yspec object.
#' @param expr An unquoted expression.
#' @param .default A named list or environment containing defaults for `expr`; 
#' consider using [ys_fill_dots()] as an alternative to passing `.default`.
#' @param .enclos An enclosing environment for evaluating `expr`.
#' 
#' @details
#' 
#' The following fields always exist in the spec and are available for 
#' querying in the filter expression:
#' - `col`:  column name `<character>`
#' - `type`: data type `<character>`; either numeric, character, or integer
#' - `discrete`: discrete data flag `<logical>`; yspec sets this to `TRUE`
#'   when the `values` field is populated
#' - `continuous`: continuous data flag `<logical>`; yspec sets this to `TRUE`
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
#' selected only if `expr` evaluates to `TRUE` (via [isTRUE()]). 
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
#' A `yspec` object
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ys_filter(spec, is.character(decode))
#' 
#' ys_filter(spec, unit == "kg" | type == "character")
#' 
#' ys_filter(spec, covariate)
#' 
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

#' Supplement data in dots list with default values
#' 
#' Please avoid thinking of this function as a way to mutate `dots`.  It sort of
#' is but not in the way you think. It really is for supplementing the spec
#' so that downstream functions are guaranteed to find these names with 
#' sensible values for every column in the spec. mutate will be a different 
#' function.
#' 
#' @param x A yspec object.
#' @param ... `name = value` data to be added to `dots`.
#' @param .overwrite Logical indicating whether `x` should be overwritten with 
#' data in `...`.
#' 
#' @details
#' By default, this function will not overwrite data in `dots` when it already 
#' exists. This behavior can be changed with the `.overwrite` argument. Note
#' that when `.overwrite` is `TRUE`, all columns in the spec will have the 
#' same value in `dots` for the `name` getting set.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' spec2 <- ys_fill_dots(spec, new_var = FALSE)
#' 
#' @return 
#' A `yspec` object with modified `.dots`
#' 
#' @md
#' @export
ys_fill_dots <- function(x, ..., .overwrite = FALSE) {
  assert_that(is_yspec(x))
  .defaults <- list(...)
  modify(x, ys_fill_dots_impl, .defaults = .defaults, .overwrite = .overwrite) 
}

#' @param x A ycol object.
#' @param .defaults A named list of default values for `dots`.
#' @param .overwrite Logical indicating whether x should be overwritten or not.
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
#' @param left A yspec object.
#' @param right A yspec object.
#' @param ... More yspec objects.
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
#' @return 
#' A single yspec object with `left`, `right` and `...` all joined together.
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

#' Rename items in a yspec object or data.frame
#' 
#' What gets renamed depends on the first argument: either `yspec` object
#' or `data.frame`. When renaming `yspec` object, use `new = old` syntax. When
#' renaming columns in `data.frame`, pass old names only in tidy select syntax; 
#' the new names come from calls to [ys_get_short()] or [ys_get_short_unit()].
#' 
#' @param .x A `yspec` object or `data.frame` to rename.
#' @param ... For the `yspec` method, pass tidy rename syntax (`new = old`); 
#' for the `data.frame` method, pass tidy select syntax to select columns to 
#' rename (new names come from the spec); see examples.
#' @param .spec A `yspec` object.
#' @param .title_case Passed to [ys_get_short()] or [ys_get_short_unit()].
#' @param .short_max Passed to [ys_get_short()] or [ys_get_short_unit()].
#' @param .unit Logical indicating if the unit should be appended to the 
#' short rename value.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ans <- ys_rename(spec, TIME = TAFD, RENAL = RF)
#' 
#' tail(ans)
#' 
#' data <- ys_help$data()
#' 
#' ys_rename(data, spec, AGE:HT)
#' 
#' @return 
#' A `yspec` object or `data.frame` with renamed columns, depending on what was
#' passed as `.x`.
#' 
#' @seealso [ys_join()], [ys_filter()], [ys_select()]
#' @md
#' @export
ys_rename <- function(.x, ... ) UseMethod("ys_rename")

#' @rdname ys_rename
#' @export
ys_rename.yspec <- function(.x, ...) {
  re <- eval_rename(expr(c(...)), as.list(.x))
  names(.x)[re] <- names(re)
  for(i in seq_along(re)) {
    .x[[re[[i]]]][["col"]] <- names(re)[[i]]
  }
  .x
}
#' @rdname ys_rename
#' @export
ys_rename.data.frame <- function(.x, .spec, ..., .title_case = TRUE, 
                                 .short_max = Inf, .unit = FALSE) {
  assert_that(is_yspec(.spec))
  re <- eval_select(expr(c(...)), .x)
  if(isTRUE(.unit)) {
    short <- ys_get_short_unit(
      .spec, title_case = .title_case, short_max = .short_max
    )  
  } else {
    short <- ys_get_short(
      .spec, title_case = .title_case, short_max = .short_max
    )
  }
  for(i in seq_along(re)) {
    names(.x)[re[i]] <- short[names(re)[i]]
  }
  .x
}

#' Recode column names in a vector
#' 
#' Use a `yspec` object to convert a vector of column names to the short
#' name with the unit optionally appended. 
#' 
#' @param col A character vector of names in `spec`. 
#' @param spec A `yspec` object.
#' @param ... Passed to [ys_get_short()] (when `unit` is `FALSE`) or 
#' [ys_get_short_unit()] (when `unit` is `TRUE`); specifically note that 
#' `title_case` and `short_max` can be passed along.
#' @param unit Logical indicating if the unit should be appended to the short 
#' name.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' x <- c("WT", "BAR", "SCR", "TIME", "FOO")
#' 
#' ys_recode(x, spec)
#' 
#' ys_recode(x, ys_select(spec, -TIME), unit = TRUE, title_case = TRUE)
#' 
#' @md
#' @export
ys_recode <- function(col, spec, ..., unit = FALSE) {
  if(is.factor(col)) col <- as.character(col)
  assert_that(is.character(col))
  assert_that(is_yspec(spec))
  if(isTRUE(unit)) {
    lookup <- unlist(ys_get_short_unit(spec, ...))
  } else {
    lookup <- unlist(ys_get_short(spec, ...))
  }
  targets <- match(col, names(lookup), nomatch = 0)
  col[targets > 0] <- lookup[targets[targets > 0]]
  col
}

#' Mutate yspec item data
#' 
#' @param x A yspec object.
#' @param ... Named lists of column data to update; only specific items can be 
#' mutated at this time; see details.
#' 
#' @details 
#' Items that can be mutated: 
#' - `short`
#' - `unit`
#' - `label`
#' 
#' @return 
#' A `yspec` object
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' xpec <- yspec:::ys_mutate(spec, TIME = list(unit = "d"), TAD = list(unit = 'w'))
#' spec$TIME
#' spec$TAD
#' 
#' @seealso [update_short()]
ys_mutate <- function(x, ...) {
  assert_that(is_yspec(x))
  args <- list(...)
  assert_that(
    is_named(args), 
    msg = "all args passed to `ys_mutate` as dots must be named"
  )
  assert_that(
    all(map_lgl(args, is.list)),
    msg = "all args passed to `ys_mutate` as dots must be lists"
  )
  cols <- names(args)
  for(i in seq_along(cols)) {
    x <- ys_mutate_impl(x, cols[[i]], args[[i]])
  }
  x
}

ys_mutate_impl <- function(x, col_name, col_new) {
  col_new <- col_new[names(col_new) %in% c("short", "unit", "label", "axis", "table")]
  if(length(col_new)==0) return(x)
  x[[col_name]] <- update_list(x[[col_name]], col_new)
  x
}
