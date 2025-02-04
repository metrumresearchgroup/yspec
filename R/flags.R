
#' @param x the flags item in `SETUP__`
#' @param valid valid column names
#' @return the flags list with column ranges expanded
#' @noRd
validate_flags <- function(x, valid) {
  if(is.null(x)) return(list())
  a <- is.list(x)
  if(!a) stop("the `flags` `SETUP__` item must be a list", call. = FALSE)
  b <- map_lgl(x, is.character)
  if(!all(b)) {
    stop("all items under `flags` in `SETUP__` must be character", call. = FALSE)  
  }
  for(i in seq_along(x)) {
    tmp <- expand_names_on_colon(x[[i]], valid)
    if(tmp[["any_bad"]]) {
      bad <- tmp[["bad_cols"]]
      bad <- paste0(" - ", bad, "\n")
      bad <- c("[yspec/flags] names not found in spec:\n", bad)
      stop(bad, call. = FALSE)
    } else {
      x[[i]] <- tmp[["cols"]]  
    }
  }
  x
}

#' @param this_col list of ycol data
#' @param name the flag name
#' @param flag_cols vector of colums that should be flagged
#' @noRd
set_this_flag_impl <- function(this_col, name, flag_cols) {
  if(is.null(this_col$dots)) this_col$dots <- list()
  if(flg_exists <- name %in% names(this_col$dots)) {
    if(!is.logical(this_col$dots[[name]])) {
      msg <- "in column `{this_col$col}`, `dots${name}` exists, but is not logical type"
      msg <- glue(msg)
      msg <- c("[yspec/flags]: ", msg, "; it will be overwritten as `FALSE`")
      warning(msg, call. = FALSE)
      this_col$dots[[name]] <- FALSE
    }
  } else {
    this_col$dots[[name]] <- this_col$col %in% flag_cols
  }
  this_col
}

#' @param x a yspec object
#' @return `x` but with dots modified with flags added
#' @noRd
add_flags <- function(x) {
  flags <- maybe_pull_meta(x, "flags")
  if(is.null(flags)) return(x)
  for(i in seq_along(flags)) {
    this_fl <- names(flags)[i]
    x <- modify(x, set_this_flag_impl, name = this_fl, flag_cols = flags[[i]])
  }
  x
}

#' Extract flags from yspec meta data
#' 
#' Use `ys_flags_chr()` to get a character vector of all columns returned by 
#' `ys_flags()`.
#' 
#' @param x a yspec object. 
#' @param ... tidy-select specification of flag names to select.
#' 
#' @return 
#' `ys_flags()` returns a named list of flag column names. `ys_flags_chr()`
#' returns a character vector of all flag columns returned by `ys_flags()`.
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' ys_flags(spec, nm)
#' 
#' ys_flags_chr(spec, nm) 
#' 
#' @seealso [ys_select_fl()]
#' 
#' @md
#' @export
ys_flags <- function(x, ...) {
  flags <- pull_meta(x, "flags") 
  if (!length(match.call(expand.dots = FALSE)$...)) {
    return(flags)
  }
  Expr <- expr(c(...))
  pos <- eval_select(expr(c(...)), flags)
  res <- flags[pos]
  names(res) <- names(pos)
  res
}

#' @md
#' @rdname ys_flags
#' @export
ys_flags_chr <- function(x, ...) {
  flags <- ys_flags(x, ...)
  unlist(flags, use.names = FALSE)
}

#' Select items from a yspec object via flags
#' 
#' @inheritParams ys_flags_chr
#' 
#' @return 
#' A yspec object containing columns named in passed through `...`. 
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' ys_select_fl(spec, nm, times)
#' 
#' @seealso [ys_flags()], [ys_flags_chr()]
#' 
#' @md
#' @export
ys_select_fl <- function(x, ...) {
  what <- ys_flags_chr(x, ...)
  ys_select(x, tidyselect::all_of(what))
}
