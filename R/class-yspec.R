
is_yspec <- function(x) inherits(x, "yspec")

##' @export
`$.yspec` <- function(x, name, ...) {
  if(!exists(name,x)) {
    return(NULL)
  }
  structure(x[[name]], class = "ycol")
}


##' @export
`[.yspec` <- function(x,i,j,drop=FALSE) {

  i <- as.character(substitute(i))
  j <- as.character(substitute(j))

  if(i=="1") {
    return(print1(x))
  }
  if(i=='2') {
    return(print2(x))
  }

  class(x) <- "list"

  if(j=="") return(x[[i]])

  dol <- substr(j,1,1)
  if(dol==".") {
    j <- sub(".", "", j, fixed=TRUE)
    return(x[[i]][["split"]][[j]])
  }

  if(nchar(j) > 0) return(x[[i]][[j]])

  stop()
}

##' @export
as.list.yspec <- function(x,...) {
  unclass(x)
}

##' @export
as.data.frame.yspec <- function(x,...) {
  out <- data.frame(col = seq_along(x),name=names(x))
  u <- lapply(x,function(xx) {
    ifelse(.has("unit",xx), xx[["unit"]], '.')
  })
  out$units <- u
  u <- lapply(x,function(xx) {
    ifelse(.has("long",xx), xx[["long"]], '.')
  })
  out$long <- u
  out
}

##' @export
head.yspec <- function(x, n = 10, ...) {
  ans <- as.data.frame.yspec(x)
  n <- min(n,nrow(ans))
  ans[seq(n),]
}

##' @export
print.yspec <- function(x,i=0,...) {
  if(i==1) {
    return(print1(x,...))
  }
  out <- as.data.frame(x)
  print.data.frame(out, row.names=FALSE,right=FALSE)
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

print2 <- function(x) {
  x <- print1(x)
  x[,c(1,3,4,2)]
}

##' Get meta data from a specification object
##'
##' @param x a yspec object
##'
##' @export
get_meta <- function(x) {
  attr(x, "meta")
}

##' Get the primary keys from a specification object
##'
##' @param x a yspec object
##'
##' @export
primary_keys <- function(x) {
  get_meta(x)[["primary_key"]]
}

##' @export
print.ycol <- function(x,...) {
  rnge <- '.'
  if(!is.null(x$range)) {
    rnge <- paste0(x$range[1], " to ", x$range[2])
  }
  x$unit <- ifelse(is.null(x$unit), '.', x$unit)
  name <- c("col", "type", "short", "unit", "range")
  values <- c(x$col, x$type, x$short, x$unit, rnge)
  ans <- data.frame(name = name, value = values)
  print(ans, row.names = FALSE, right = FALSE)
}

##' @export
as.list.ycol <- function(x,...) {
  unclass(x)
}

data_stem <- function(x) {
  m <- get_meta(x)
  file <- basename(m[["yml_file"]])
  gsub(".ya?ml$", "", file)
}
