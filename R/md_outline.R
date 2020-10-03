# nocov start

##' DEPRECATED: Generate an outline representation of a data spec
##'
##' @param x a yspec object
##'
##' @return character
##'
##' @export
md_outline <- function(x) { 

  stop("md_outline is deprecated")
  
  assertthat::assert_that(is_yspec(x))

  txt <- lapply(x, define_col_1)

  purrr::flatten_chr(txt)

}

define_col_1 <- function(x) {
  unit <- NULL
  source <- NULL
  comment <- NULL
  decode <- NULL
  col <- x$col

  col <- paste0("1.  __`", col, "`__")
  short <- pack_short(x)
  descr  <- pack_long(x)
  unit <- pack_unit(x)
  source <- pack_source(x)
  comment <- pack_comment(x)
  type <- pack_type(x)

  values <- define_values(x)

  return(c(col,descr,short,type,values,unit,source,comment))
}
#nocov end

has_values <- function(x) {
  if(!is.null(x$values)) {
    if(length(x$values) > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

has_decode <- function(x) {
  if(.has("decode",x)) {
    if(length(x$decode) > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

has_unit <- function(x) {
  !is.null(x$unit)
}

has_source <- function(x) {
  !is.null(x$source)
}

has_comment <- function(x) {
  !is.null(x$comment)
}

define_values <- function(x) {

  if(.no("values", x)) return(NULL)

  long <- x[["longvalues"]]
  decode <- NULL
  values <- NULL

  if(has_decode(x)) {
    decode <- x$decode
  }

  if(has_values(x)) {
    values <- x$values
  }

  if(has_decode(x)) {
    values <- paste0(values, " = " , decode)
  }

  if(is.null(values)) return(character(0))

  if(!long) {
    values <- paste0("`", values, "`")
    values <- paste(values, collapse=", ")
    values <- sub("\\, *$", "", values)
    values <- paste0("    - values: ",values)
    return(values)
  }

  values <- paste0("- `", values,"`")

  c("    - values:", paste0("        ",values))
}


pack_unit <- function(x) {
  if(.no("unit", x)) return(NULL)
  paste0("    - unit: `", x$unit,"`")
}

pack_source <- function(x) {
  if(.no("source", x)) return(NULL)
  paste0("    - source: `", x$source,"`")
}

pack_comment <- function(x) {
  if(.no("comment", x)) return(NULL)
  paste0("    - comment: ", x$comment)
}

pack_short <- function(x) {
  if(.no("short", x)) return(NULL)
  if(x$short==x$col) return(NULL)
  paste0("    - short name: ", x$short)
}

pack_long <- function(x) {
  if(.no("long", x)) return(NULL)
  paste0("    - ", x$long)
}

pack_type <- function(x) {
  if(.no("type", x)) return(NULL)
  paste0("    - ", x$type)
}


