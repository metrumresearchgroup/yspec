
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
  out <- data.frame(col = seq_along(object), name = names(object), stringsAsFactors = FALSE)
  type <- map_chr(object, "type", .default = ".")
  out$c <- ifelse(type=="character", "+", "-")
  dec <- map(object, "decode") %>% unname %>% map_int(length)
  out$d <- ifelse(dec > 0, "+", "-")
  out$unit <- map_chr(object, "unit", .default = ".")
  out$short <- map_chr(object, "short", .default = ".")
  out$source <- map_chr(object, "lookup_source", .default='.')
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
#' Use this to scavenge a data frame for columns that you want to keep. Do not
#' use this for final column selection; use [dplyr::select()] instead. 
#' 
#' @param data a data frame with at least one column that is found in `spec`
#' @param spec a `yspec` object
#' @param report if `TRUE`, report missing columns
#' 
#' @examples
#' data <- ys_help$data()
#' spec <- ys_help$spec()
#' data$STUDY <- NULL
#' 
#' head(ys_prune(data, spec))
#' head(ys_prune(data, spec, report = TRUE))
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
#' A data frame with common columns with `spec`, in the order they appear
#' in `spec`. 
#'   
#' @md
#' @export
ys_prune <- function(data, spec, report = FALSE) {
  assert_that(is.data.frame(data))
  assert_that(is_yspec(spec))
  grab <- intersect(names(data), names(spec))
  if(length(grab)==0) {
    stop("there are no names common between `data` and `spec`", call. = FALSE)  
  }
  grab <- order(match(grab, names(spec)))
  if(isTRUE(report)) {
    missing <- setdiff(names(spec), names(data))
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
