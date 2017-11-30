

has_unit <- function(x) !is.na(x$unit)

make_axis_label <- function(x) {
  unit <- NULL
  if(has_unit(x)) {
    unit <- paste0(" (", x$unit,")")
  }
  x$axis_label <- paste0(x$short, unit)
  x
}

make_col_label <- function(x) {
  x$col_label <- paste0(x$col, "//", x$axis_label)
  x$plot_data <- c(x$col, x$axis_label)
  x
}





.get <- function(x,what,default) {
  what <- as.character(substitute(what))
  if(!is.null(x[[what]])) return(x[[what]])
  return(default)
}
.no <- function(x,y) is.null(x[[as.character(substitute(y))]])
.has <- function(x,y) !is.null(x[[as.character(substitute(y))]])

unpack_col <- function(x) {
  if(!exists("about",x)) {
    x[["about"]] <- c(x[["col"]],NA)
  }
  about <- x[["about"]][1:2]
  x[["about"]] <- NULL
  x[["short"]] <- as.character(about[1])
  if(!is.na(about[2])) {
    x[["unit"]] <- as.character(about[2])
  }
  x$continuous <- .has(x,range)
  if(x$continuous) {
    x$range <- unlist(x$range, use.names=FALSE)
  }
  x$discrete <- !x$continuous
  if(.no(x,"longvalues")) x[["longvalues"]] <- FALSE
  if(x$discrete) {
    if(!.has(x,decode)) {
      x$decode <- names(x$values)
    }
    x$values <- unlist(x$values, use.names=FALSE)
  }
  x <- make_axis_label(x)
  x <- make_col_label(x)
  x$is_split <- FALSE
  if(exists("split", x)) {
    x$is_split <- TRUE
    x$split <- unpack_split(x$split,x$col)
  }
  x
}


load_lookup_spec <- function(x) {
  if(!exists("lookup_file",x)) return(list())
  if(!file.exists(x$lookup_file)) {
    stop("Couldn't find lookup file", call. = FALSE)
  }
  message("Using lookup file: ", basename(x$lookup_file))
  yaml.load_file(x$lookup_file)
}


unpack_spec <- function(x) {
  meta <- list()
  metai <- grepl("Options__", names(x), fixed = TRUE)
  if(any(metai)) {
    meta <- x[[which(metai)]]
    x <- x[!metai]
  }
  if(exists("lookup_file", meta)) {
    meta[["lookup_file"]] <- normalizePath(meta[["lookup_file"]])
  }
  lookup <- load_lookup_spec(meta)
  cols <- names(x)
  for(i in seq_along(x)) {
    col <- cols[i]
    def <- list(lookup = col)
    if(is.null(x[[i]])) {
      x[[i]] <- list()
    }
    x[[i]] <- merge.list(def,x[[i]], open = TRUE)
    if(exists(x[[i]][["lookup"]],lookup)) {
      x[[i]] <- merge.list(lookup[[x[[i]][["lookup"]]]],x[[i]], open = TRUE)
    }
    x[[i]][["col"]] <- col
    x[[i]] <- unpack_col(x[[i]])
  }
  structure(x,class="yspec",meta = meta)
}




##' @export
get_meta <- function(x) {
  attr(x, "meta")
}

##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @export
load_spec <- function(file) {
  x <- yaml::yaml.load_file(file)
  unpack_spec(x)
}


##' @export
summary.yspec <- function(object, ... ) {
  n <- length(object)
  cat(paste0("There are ", n, " columns in the spec"))
  return(invisible(NULL))

}

unpack_split_col <- function(x) {
  x[["about"]] <- x[["about"]][c(1,2)]
  x[["short"]] <- x[["about"]][1]
  x[["unit"]] <- x[["about"]][2]
  if(!is.character(x[["when"]])) x[["when"]] <- NA
  x <- make_axis_label(x)
  x <- make_col_label(x)
  x
}

unpack_split <- function(x,col) {
  for(n in names(x)) {
    x[[n]][["col"]] <- col
    x[[n]][["name"]] <- n
    x[[n]] <- unpack_split_col(x[[n]])
  }
  x
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
    ifelse(.has(xx,"unit"), xx[["unit"]], '.')
  })
  out$units <- u
  u <- lapply(x,function(xx) {
    ifelse(.has(xx,"long"), xx[["long"]], '.')
  })
  out$long <- u
  out
}

##' @export
print.yspec <- function(x,i=0,...) {
  if(i==1) {
    return(print1(x,...))
  }
  out <- as.data.frame(x)
  print.data.frame(out, row.names=FALSE,right=FALSE)
}


print1 <- function(x,...) {
  out <- lapply(x, function(xx) {
    decode <- '.'
    values <- '.'
    unit <- '.'
    if(.has(xx,"decode")) decode <- xx$decode
    if(.has(xx,"values")) values <- xx$values
    if(.has(xx,"unit")) unit <- xx$unit
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

#
# `[[.yspec` <- function(x,i,j,..., exact=TRUE) {
#
#   i <- as.character(substitute(i))
#   j <- as.character(substitute(j))
#
#   class(x) <- "list"
#
#   if(j=="") return(x[[i]])
#   if(nchar(j) > 0) return(x[[i]][["split"]][[j]])
#   stop()
# }


