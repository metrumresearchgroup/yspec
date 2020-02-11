
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


#' Add extra column elements to a yspec object
#' 
#' @param x a `yspec` object
#' @param y a `yspec` object
#' @param ... not used
#' 
#' 
#' @md
#' @export
c.yspec <- function(x,y,...) {
  assert_that(is_yspec(y))
  new_cols <- setdiff(names(y),names(x))
  if(!identical(new_cols, names(y))) {
    stop("'x' and 'y' cannot share any names.")  
  }
  structure(c(as.list(x),as.list(y)), meta = get_meta(x), class="yspec")
}

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
label.yspec <- function(x,default="short",...) {
  map_chr(x,label.ycol, default = default)  
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
  for(i in seq_len(ncol(data))) {
    attr(data[[i]],"label") <- col_labels[[i]]
  }
  data
}
