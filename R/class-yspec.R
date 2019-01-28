
is_yspec <- function(x) inherits(x, "yspec")

##' @export
`$.yspec` <- function(x, name, ...) {
  if(!exists(name,x)) {
    return(NULL)
  }
  x[[name]]
}

##' @export
`[.yspec` <- function(x,i,j,drop=FALSE) {
  meta <- get_meta(x)
  x <- unclass(x)
  x <- x[i]
  structure(x, class = "yspec", meta = meta)
}

##' @export
`[[.yspec` <- function(x,i,..., exact = TRUE) {
  unclass(x)[[i]]
}

##' @method as.list yspec
##' @export
as.list.yspec <- function(x,...) {
  lapply(unclass(x),unclass)
}

##' @export
as.data.frame.yspec <- function(x,...) {
  summary.yspec(x,...)
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
print.yspec <- function(x,i=0,...) {
  # if(i==1) {
  #   return(print1(x,...))
  # }
  out <- summary.yspec(x)
  print.data.frame(out, row.names=FALSE, right=FALSE)
}

yml_rm <- function(x) {
  if(is.null(x)) return("")
  gsub("\\.ya?ml$", "", x)
}

##' @export
summary.yspec <- function(object, ...) {
  out <- data.frame(col = seq_along(object), name=names(object))
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
# 
# print1 <- function(x,...) {
#   out <- lapply(x, function(xx) {
#     decode <- '.'
#     values <- '.'
#     unit <- '.'
#     if(.has("decode",xx)) decode <- xx$decode
#     if(.has("values",xx)) values <- xx$values
#     if(.has("unit",xx)) unit <- xx$unit
#     data.frame(col=xx$col,
#                unit=unit,
#                value=values,
#                decode=decode,
#                stringsAsFactors=FALSE)
#   })
#   out <- as.data.frame(do.call('rbind',out),stringsAsFactors=FALSE)
#   rownames(out) <- NULL
#   return(out)
# }

##' Get meta data from a specification object
##'
##' @param x a yspec object
##'
##' @export
get_meta <- function(x) {
  ans <- attr(x, "meta")
  if(is.null(ans)) {
    .stop("The object does not have a meta attribute.")
  }
  ans
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
unit.yspec <- function(x,default = '.',...) {
  map_chr(x,"unit", .default = default)
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

short <- function(x,...) UseMethod("short")
##' @export
short.ycol <- function(x, default = ".", ...) {
  if(.no("short", x)) {
    return(default)
  }
  x[["short"]]
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