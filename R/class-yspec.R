
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
  out <- data.frame(col = seq_along(x), name=names(x))
  out$type <- map_chr(x, "type", .default = ".")
  out$unit <- map_chr(x, "unit", .default = ".")
  out$short <- map_chr(x, "short", .default = ".")
  out
}

##' @export
head.yspec <- function(x, n = 10, ...) {
  ans <- as.data.frame.yspec(x)
  head(ans, n = n, ...)
}

##' @export
tail.yspec <- function(x, n = 10, ...) {
  ans <- as.data.frame.yspec(x)
  tail(ans, n = n, ...)
}

##' @export
print.yspec <- function(x,i=0,...) {
  if(i==1) {
    return(print1(x,...))
  }
  out <- as.data.frame(x)
  print.data.frame(out, row.names=FALSE, right=FALSE)
}

##' @export
summary.yspec <- function(object, ...) {
  .vars <- quos(...)
  vars <- select_vars(names(object),!!!.vars)
  if(length(vars)==0) {
    vars <- names(object)[seq_len(min(10,length(object)))]
  }
  for(v in vars) {
    x <- object[[v]]
    unit <- dplyr::if_else(is.null(x$unit), "no-unit", x$unit)
    unit <-paste0( "(", unit, ")")
    cat(paste0(x$short, " ", unit, "\n"))
    cat(paste0("  type: ", x$type, "\n"))
    if(is.numeric(x$range)) {
      cat(paste0("  range: [",x$range[1], ', ', x$range[2], "]\n"))
    }
    cat("------\n")
  }
}

print1 <- function(x,...) {
  out <- lapply(x, function(xx) {
    decode <- '.'
    values <- '.'
    unit <- '.'
    if(.has("decode",xx)) decode <- xx$decode
    if(.has("values",xx)) values <- xx$values
    if(.has("unit",xx)) unit <- xx$unit
    data.frame(col=xx$col,
               unit=unit,
               value=values,
               decode=decode,
               stringsAsFactors=FALSE)
  })
  out <- as.data.frame(do.call('rbind',out),stringsAsFactors=FALSE)
  rownames(out) <- NULL
  return(out)
}

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

##' @rdname get_meta
##' @export
ys_spec_file <- function(x) {
  get_meta(x)[["spec_file"]]  
}

##' Get the primary keys from a specification object
##'
##' @param x a yspec object
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

get_lookup_files <- function(x) {
  ans <- get_meta(x)[["lookup_file"]]
  if(is.character(ans)) {
    return(ans)
  }
  return(character(0))
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