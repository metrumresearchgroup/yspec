
is_yspec <- function(x) inherits(x, "yspec")

##' @export
update.yspec <- function(object, projectnumber=NULL, sponsor=NULL, ...) {
  
  non_null <- c(projectnumber,sponsor)
  
  if(is.null(non_null)) return(object)
  
  m <- get_meta(object)
  
  if(is.character(projectnumber)) {
    m[["projectnumber"]] <- projectnumber  
  }
  
  if(is.character(sponsor)) {
    m[["sponsor"]] <- sponsor  
  }
  
  structure(object, meta = m)
  
}

#' Update short names in a yspec object
#' 
#' @param spec a yspec object
#' @param ... `<column name> = <short name>` pairs
#' 
#' @details
#' If no update items are passed in, the original spec object will be returned.
#' An error will be issued if a column update is requested, but can't be found
#' in the spec.
#' 
#' @examples
#' sp <- ys_help$spec()
#' 
#' sp2 <- update_short(sp, ID = "subject", ALB = "serum albumin")
#' 
#' @export
update_short <- function(spec, ...) {
  assert_that(is_yspec(spec),msg="'spec' must be a yspec object")
  short <- list(...)
  if(length(short)==0) return(spec)
  col <- names(short)
  for(i in seq_along(short)) {
    this_col <- col[[i]]
    assert_that(
      exists(this_col,spec), 
      msg = glue("column '{this_col}' does not exist in the spec object")
    )
    spec[[col[i]]][["short"]] <- short[[i]]    
  }
  spec
}



#' Add extra column elements to a yspec object
#' 
#' @param x a `yspec` object
#' @param y a `yspec` object
#' @param ... not used
#' @param .meta meta information to use for the new spec object
#' 
#' 
#' @md
#' @export
c.yspec <- function(x, y, ... ,.meta = get_meta(x)) {
  assert_that(is_yspec(y))
  new_cols <- setdiff(names(y),names(x))
  if(!identical(new_cols, names(y))) {
    stop("`x` and `y` cannot share any names.")  
  }
  structure(c(unclass(x),unclass(y)), meta = .meta, class = "yspec")
}

##' @export
`$.yspec` <- function(x, name, ...) {
  if(!exists(name, x)) {
    return(NULL)
  }
  x[[name]]
}

##' @export
`[.yspec` <- function(x, i, j, drop = FALSE) {
  meta <- get_meta(x)
  x <- unclass(x)
  x <- x[i]
  structure(x, class = "yspec", meta = meta)
}

##' @export
`[[.yspec` <- function(x, i, ..., exact = TRUE) {
  unclass(x)[[i]]
}

##' @method as.list yspec
##' @export
as.list.yspec <- function(x, ...) {
  lapply(unclass(x), unclass)
}

##' @export
as.data.frame.yspec <- function(x, ...) {
  summary.yspec(x, ...)
}

##' @export
head.yspec <- function(x, n = 10, ...) {
  ans <- summary.yspec(x)
  head(ans, n = n, ...)
}

##' @export
tail.yspec <- function(x, n = 10, ...) {
  ans <- summary.yspec(x)
  tail(ans, n = n, ...)
}

##' @export
print.yspec <- function(x, i = 0, ...) {
  out <- summary.yspec(x)
  print.data.frame(out, row.names = FALSE, right = FALSE)
}

yml_rm <- function(x) {
  if(is.null(x)) return("")
  gsub("\\.ya?ml$", "", x)
}

##' @export
summary.yspec <- function(object, ...) {
  out <- data.frame(
    col = seq_along(object), 
    name = names(object), 
    stringsAsFactors = FALSE
  )
  type <- map_chr(object, "type", .default = ".")
  type <- ifelse(type=="character", "c", "-")
  dec <- unname(map(object, "decode"))
  dec <- map_int(dec, length)
  dec <- ifelse(dec > 0, "d", "-")
  out$info <- paste0(type, dec)
  out$unit <- map_chr(object, "unit", .default = ".")
  out$short <- map_chr(object, "short", .default = ".")
  out$source <- map_chr(object, "lookup_source", .default = '.')
  ext <- map_chr(object, "extended_from", .default = ".")
  if(any(ext != ".")) {
    out$source[ext != '.'] <- ext[ext != '.']
    out$info <- paste0(out$info, ifelse(ext == '.', "-", "e"))
  } else {
    out$info <- paste0(out$info, "-")  
  }
  out$source <- sub("\\.ya?ml$", "", out$source)
  out$col <- NULL
  out
}

#' Get meta data from a specification object
#'
#' @param x a yspec object
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' ans <- get_meta(spec)
#' 
#' @export
get_meta <- function(x) {
  ans <- attr(x, "meta")
  if(is.null(ans)) {
    .stop("the object does not have a meta attribute.")
  }
  ans
}

#' Pull a single item from the meta data object
#' 
#' @param x a yspec object
#' @param what character name of item in meta
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' pull_meta(spec, "description")
#' 
#' @export
pull_meta <- function(x,what) {
  ans <- attr(x, "meta")
  assert_that(exists(what,ans))
  ans[[what]]
}

maybe_pull_meta <- function(x, what) {
  attr(x, "meta")[[what]]  
}

##' Get the file name for a yspec object
##' 
##' This read from the yspec object meta data via [get_meta].
##' 
##' @inheritParams get_meta
##' 
##' @examples
##' spec <- load_spec_ex()
##' ys_spec_file(spec)
##' 
##' @md
##' @export
ys_spec_file <- function(x) {
  get_meta(x)[["spec_file"]]  
}

##' Get the primary keys from a specification object
##'
##' @param x a yspec object
##'
##' @examples
##' primary_keys(load_spec_ex())
##'
##' @export
primary_keys <- function(x) {
  get_meta(x)[["primary_key"]]
}

data_stem <- function(x) {
  m <- get_meta(x)
  file <- basename(m[["spec_file"]])
  gsub(".ya?ml$", "", file)
}

unit <- function(x,...) UseMethod("unit")
##' @export
unit.ycol <- function(x, default = '.',...) {
  if(is.null(x[["unit"]])) {
    return(default)
  }
  x[["unit"]]
}
##' @export
unit.yspec <- function(x,default = '.', .aslist = TRUE, ...) {
  if(isTRUE(.aslist)) {
    ans <- map(x, "unit", .default = default)
  } else {
    ans <-  map_chr(x, "unit", .default = default) 
  }
  ans
}

long <- function(x,...) UseMethod("long")
##' @export
long.ycol <- function(x, default = '.', ... ) {
  if(is.null(x[["long"]])) {
    return(default)
  }
  x[["long"]]
}
##' @export
long.yspec <- function(x, default = '.', ...) {
  map_chr(x,"long", .default = default)
}

label <- function(x,...) UseMethod("label")
#' @export
label.ycol <- function(x, default = 'short', ...) {
  if(.has("label",x)) return(x[["label"]])
  if(.has("long",x)) {
    if(nchar(x[["long"]]) <= 40) {
      return(x[["long"]])  
    }
  }
  return(x[[default]])
}
#' @export
label.yspec <- function(x,default="short",.aslist=TRUE,...) {
  if(isTRUE(.aslist)) {
    ans <- map(x, label.ycol, default = default)
  } else {
    ans <- map_chr(x, label.ycol, default = default)
  }
  ans
}

type <- function(x,...) UseMethod("type")
##' @export
type.ycol <- function(x, default = "numeric", ... ) {
  if(is.null(x[["type"]])) {
    return(default)
  }
  x[["type"]]
}
##' @export
type.yspec <- function(x, default = "numeric",...) {
  map_chr(x,"type", .default = default)
}

#' Form a short name
#' 
#' @param x a yspec or ycol object
#' @param default character field name to use when the `short` field is not 
#' found
#' @param short_max if `short` is found, but contains more than `short_max` 
#' characters, `col` will be used
#' @param title_case `short` will be converted with [tools::toTitleCase] if it 
#' is found
#' @param ... passed to methods
#' @md
short <- function(x,...) UseMethod("short")
#' @rdname short
#' @export
short.ycol <- function(x, default = "col", 
                       short_max = getOption("yspec.short.max",NULL), 
                       title_case = getOption("yspec.short.title", FALSE), ...) {
  if(.no("short", x)) {
    ans <- x[[default]]
  } else {
    ans <- x[["short"]]
    if(isTRUE(title_case)) {
      ans <- toTitleCase(ans)  
    }
  }
  if(is.numeric(short_max)) {
    if(nchar(ans) > short_max) {
      ans <- x[["col"]]
    }
  }
  ans
}

#' @param .aslist if `TRUE`a named list is returned; otherwise, a named character
#' vector
#' @rdname short
#' @export
short.yspec <- function(x, default = "short", .aslist=TRUE,...) {
  if(isTRUE(.aslist)) {
    ans <- map(x, short.ycol, default = default, ...)  
  } else {
    ans <- map_chr(x, short.ycol, default = default, ...)      
  }
  ans
}

comment <- function(x,...) UseMethod("comment")
##' @export
comment.ycol <- function(x, default = ".",...) {
  if(.no("comment",x)) {
    return(default)
  }
  x[["comment"]]
}
##' @export
comment.yspec <- function(x,default = '.',...) {
  map_chr(x, "comment", .default = default)
}

Range <- function(x,...) UseMethod("Range")
##' @export
Range.ycol <- function(x, default = '.', ...) {
  if(.no("range",x)) {
    return(default)
  }
  paste(x[["range"]], collapse = " to ")
}
##' @export
Range.yspec <- function(x, default = '.', ...) {
  map_chr(x, Range.ycol)
}

lookup <- function(x,...) UseMethod("lookup")

##' @export
lookup.ycol <- function(x,...) {
  if(.has("lookup",x)) {
    return(x[["lookup"]])
  }
  x[["col"]]
}

Source <- function(x,...) UseMethod("Source")
##' @export
Source.ycol <- function(x, default = ".",...) {
  if(.no("source",x)) {
    return(default)
  }
  x[["source"]]
}
##' @export
Source.yspec <- function(x,default = '.',...) {
  map_chr(x, "source", .default = default)
}

##' Get the yaml file location for a spec object
##' 
##' @param x yspec object
##' @param ... not used
##' @export
yspec_yml_file <- function(x,...) UseMethod("yspec_yml_file")

##' @export
yspec_yml_file.default <- function(x,...) {
  get_meta(x)[["spec_file"]]  
}

#' Add label attribute to data set columns
#' 
#' 
#' @param data a `data.frame` to label
#' @param spec yspec object for `data`
#' @param fun the function to use for forming `label`
#' 
#' @details
#' An error is generated if the names of `data` are not identical to names 
#' of `spec`. 
#' 
#' If the user passes `fun` to generate a custom label, the function must take
#' a single argument, the column `ycol` object, and must return the label for 
#' that column as a character vector of length one.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' data <- ys_help$data()
#' 
#' data <- ys_add_labels(data,spec)
#' 
#' sapply(data,attr,"label")
#' 
#' str(data[,1:5])
#' 
#' @md
#' @export
ys_add_labels <- function(data,spec,fun=label.ycol) {
  assert_that(inherits(data,"data.frame"))
  assert_that(inherits(spec,"yspec"))
  assert_that(identical(names(data),names(spec)))
  col_labels <- map_chr(spec,fun)
  for(i in seq_along(data)) {
    attr(data[[i]],"label") <- col_labels[[i]]
  }
  data
}


#' Prune a data frame, keeping columns in a yspec object
#' 
#' Use this to scavenge a data frame for columns that you want to keep. Select 
#' additional columns through `...`. Do not use this for final column selection; 
#' use [dplyr::select()] instead. 
#' 
#' @param data A data frame with at least one column that is found in `spec`.
#' @param spec A `yspec` object.
#' @param ... Additional columns carry into the output, specified using 
#' tidy-select syntax.
#' @param .report If `TRUE`, report missing columns.
#' 
#' @examples
#' data <- ys_help$data()
#' spec <- ys_help$spec()
#' data$STUDY <- NULL
#' 
#' head(ys_prune(data, spec))
#' head(ys_prune(data, spec, .report = TRUE))
#' 
#' data$FOO <- 1
#' data$BAR <- 2
#' data$YAK <- 3
#' 
#' head(ys_prune(data, spec, YAK, FOO))
#' 
#' # Use this for final subsetting
#' # It will fail if all the columns aren't there
#' data <- ys_help$data()
#' head(dplyr::select(data, names(spec)))
#'  
#' @details
#' An error is generated if there are no columns in common between `data` and 
#' `spec`. 
#' 
#' @return 
#' A data frame with common columns with `spec` and `...`, in the order they 
#' appear in `spec`. 
#'   
#' @md
#' @export
ys_prune <- function(data, spec, ..., .report = FALSE) {
  assert_that(is.data.frame(data))
  assert_that(is_yspec(spec))
  # spec positions for matching names in the data set
  target <- names(spec)
  re <- eval_select(expr(c(...)), data)
  target <- unique(c(target, names(re)))
  igrab <- sort(match(names(data), target), na.last = NA)
  if(length(igrab)==0) {
    stop("there are no names common between `data` and `spec`", call. = FALSE)  
  }
  # convert igrab to names in spec, ordered by spec; this is what we'll take
  grab <- target[igrab]
  if(isTRUE(.report)) {
    missing <- setdiff(target, names(data))
    for(col in missing) {
      message("Column not found: ", col)  
    }
  }
  data[, grab, drop = FALSE]
}

as_spec_list <- function(...) {
  x <- list(...)
  names(x) <- map_chr(map(x,get_meta),"name")
  cl <- purrr::map_lgl(x, inherits, "yspec")
  assert_that(all(cl))
  structure(x,class="spec_list")
}

is.spec_list <- function(x) inherits(x,"spec_list")

#' Extract vector of values from discrete data columns. 
#' 
#' @param x a yspec object. 
#' @param select decode labels or inter positions to retain using tidyselect 
#' syntax; it is impossible to select items based on the `value` itself; 
#' see **Details**.
#' 
#' @return 
#' The `yspec` method returns a named list of all column values data; non-discrete
#' data columns are skipped. The `ycol` method returns a vector of values which is 
#' named of `decodes` were provided. `ys_mget_values()` returns a named list of
#' values for each column passed under `...`. See **Examples*.
#' 
#' @details
#' Only discrete column types can be targeted for extracting values. When 
#' extracting with `ys_get_values()`, the default behavior is to return 
#' everything in the `values` list. When calling the `ycol` method for 
#' `ys_get_values()`, the `select` argument can be used to limit which 
#' `values` are returned. This selection can always be done by integer position
#' (e.g., pass `-1` to drop the first position). When `decodes` are available, 
#' the selection can also be done based on the decode value, but in this case, 
#' the selection _can't_ be done according to the `value`. When there are no 
#' decodes available, then yspec will take the values to be the decodes also, 
#' allowing the user to select based on those `values`. Note that, in this 
#' case, the selection is always done passing `character` data to `select`, 
#' not `numeric``. Please see the **Examples**.
#' 
#' @examples
#' spec <- ys_help$spec()
#' 
#' spek <- ys_select(spec, RF, WT, STUDY, AGE, CP) 
#' ys_get_values(spek)
#' 
#' ys_get_values(spec$RF)
#' 
#' ys_get_values(spec$RF, -1)
#' 
#' ys_get_values(spec$RF, -Severe)
#' 
#' ys_get_values(spec$RF, Moderate)
#' 
#' ys_get_values(spec$STUDY, everything())
#' 
#' # This drops `value = 0` (the first position) rather than `value = 1` (the second position)
#' ys_get_values(spec$BLQ, -1)
#' 
#' # Examples when there are no decodes
#' spec$PHASE$values <- c(300, 200, 100)
#' spec$PHASE$decodes <- NULL
#' 
#' ## select 300
#' ys_get_values(spec$PHASE, 1)
#' 
#' ## also selects 300
#' ys_get_values(spec$PHASE, "300")
#' 
#' ## Don't select 100
#' ys_get_values(spec$PHASE, -3)
#' 
#' ## Also don't select 100
#' ys_get_values(spec$PHASE, -"100")
#' 
#' ys_get_values(spec$STUDY, 2)
#' 
#' ys_mget_values(spec, RF = -Severe, EVID = -1, MDV = everything())
#' 
#' ys_get_values(spec$BLQ, -`below QL`)
#'  
#' ys_get_values(spec$BLQ, contains("below"))  
#' 
#' @export
ys_get_values <- function(x, ...) UseMethod("ys_get_values")

#' @rdname ys_get_values
#' @export
ys_get_values.yspec <- function(x, ...) {
  dis <- keep(x, ~ isTRUE(.x$discrete))
  map(dis, ys_get_values, ...)
}

#' @rdname ys_get_values
#' @export
ys_get_values.ycol <- function(x, select = everything(), ...) {
  if(!isTRUE(x$discrete)) {
    abort(glue::glue("column `{col}` is not discrete."))  
  }
  ans <- x$values
  if(is.null(x$decode)) {
    decode <- ans
  } else {
    decode <- x$decode  
  }
  names(ans) <- decode
  selected <- tidyselect::eval_select(enquo(select), ans)
  ans <- ans[selected]
  if(!length(ans)) {
    abort("no values were selected.")  
  }
  return(ans)
  ans
}

#' @param ... named items to pass to `ys_get_values()` as `select`.
#' @rdname ys_get_values
#' @export
ys_mget_values <- function(x, ...) {
  dots <- exprs(...)
  out <- vector(mode = "list", length = length(dots))
  for(i in seq_along(dots)) {
    col <- names(dots)[i]
    w <- ys_select(x, all_of(col))
    val <- ys_get_values(w[[1]], !!dots[[i]])
    out[[i]] <- val
  }
  names(out) <- names(dots)
  out
}
