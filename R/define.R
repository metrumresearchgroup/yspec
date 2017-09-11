
spc1 <- "    "
spc2 <- "        "

has_values <- function(x) {
  if(!is.null(x$values)) {
    if(length(x$values) > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

has_decode <- function(x) {
  if(.has(x,decode)) {
    if(length(x$decode) > 0) {
      return(TRUE)
    }
  }
  return(FALSE)
}

has_unit <- function(x) {
  if(is.null(x$unit)) return(FALSE)
  return(TRUE)
}

has_source <- function(x) {
  if(is.null(x$source)) return(FALSE)
  !is.na(x$source)
}

has_comment <- function(x) {
  if(is.null(x$comment)) return(FALSE)
  return(TRUE)
}

define_values <- function(x) {

  if(.no(x,"values")) return(NULL)
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
  if(.no(x,"unit")) return(NULL)
  paste0("    - unit: `", x$unit,"`")
}

pack_source <- function(x) {
  if(.no(x,"source")) return(NULL)
  paste0("    - source: `", x$source,"`")
}

pack_comment <- function(x) {
  if(.no(x,"comment")) return(NULL)
  paste0("    - comment: ", x$comment)
}

pack_short <- function(x) {
  if(.no(x,"short")) return(NULL)
  if(x$short==x$col) return(NULL)
  paste0("    - short name: ", x$short)
}

pack_long <- function(x) {
  if(.no(x,"long")) return(NULL)
  paste0("    - ", x$long)
}

pack_derivation <- function(x) {
  if(is.null(x$derivation)) return(NULL)
  x <- x$derivation
  x <- paste0("      `", x, "`")
  if(length(x) > 1) x <- paste(x, collapse = '; ')
  c("    - derivation: ", x)
}

pack_if_missing <- function(x) {
  if(is.null(x$if_missing)) return(NULL)
  paste0("    - if_missing: ", x$if_missing)
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
  derivation <- pack_derivation(x)
  miss <- pack_if_missing(x)
  source <- pack_source(x)
  comment <- pack_comment(x)

  values <- NULL
  if(.has(x,"values")) {
    values <- define_values(x)
  }
  if(x$is_split) {
    values <- pack_split(x)
    values <- paste0("        ", values)
  }
  return(c(col,descr,short,values,unit,derivation,miss,source,comment))
}


md_header <- function(file="tran2.xpt",...) {
  c("---",
    "title: ''",
    "author: ''",
    "date: ''",
    "fontsize: 12pt",
    "---\n",
    "## Data source",
    paste0("  * File: ", file),
    paste0("  * Date: ", Sys.Date(), "\n"),
    "## Data definitions\n")
}


md_outline <- function(x, head = TRUE,...) {

  if(!is.null(head)) head <- md_header(...)

  cols <- vector(mode="list", length(x))

  for(i in seq_along(x)) {
    cols[[i]] <- define_col_1(x[[i]])
  }

  c(head, unlist(cols, use.names=FALSE))
}


##' Render a data specification object
##'
##' @param x object
##' @param stem for output file name
##' @param format function defining how to render the object
##' @param output_format passed to \code{rmarkdown::render}
##' @param ... passed to \code{rmarkdown::render}
##'
##' @export
render_spec <- function(x,
                        stem,
                        format = c("md_outline"),
                        output_format="pdf_document",...) {

  format <- match.arg(format)

  format_fun <- get(format, mode="function")

  file <- paste0(stem, ".Rmd")

  if(file.exists(file)) file.remove(file)

  cat(format_fun(x,...), file=file, sep="\n")

  rmarkdown::render(file,output_format=output_format,...)
}



pack_split <- function(sp) {
  if(!exists("split",sp)) return(character(0))
  sp <- sp$split
  short <- sapply(sp, `[[`, "short")
  unit <- sapply(sp, `[[`, "unit")
  unit[is.na(unit)] <- ""
  unit[nchar(unit)>1] <- paste0("`",unit[nchar(unit)>1],"`")
  when <- sapply(sp, `[[`, "when")
  when[is.na(when)] <- ""
  p <- paste0("- ", short," ", unit)
  p[when !=""] <- paste0(p[when!=""], " when `", when[when!=""], "`")
  p
}


##' Render a define.pdf document
##'
##' @param file yaml file identifying specifications
##' @param title a title for the define document
##' @param output character stem to create a name for the output file
##' @param ... passed to \code{rmarkdown::render}
##'
##' @details
##' \code{output} should not include a file extension.
##'
##' @export
render_define <- function(file, title, output, ...) {

  output <- paste0(output, ".md")

  x <- yaml.load_file(file)
  files <- names(x)
  n_files <- length(x)
  path <- '.'

  for(i in seq_along(x)) {
    x[[i]]$name <- files[[i]]
    if(is.null(x[[i]]$spec)) x[[i]]$spec <- paste0(files[[i]], ".yml")
    if(is.null(x[[i]]$source)) x[[i]]$source <- paste0(x[[i]]$name, ".xpt")
    if(is.null(x[[i]]$path)) x[[i]]$path <- path
    x[[i]]$file <- file.path(x[[i]]$path, x[[i]]$spec)
    if(!file.exists(x[[i]]$file)) stop("could not find file ", x[[i]]$file)
  }

  template_title <- '---
title: <insert-title>
author: ""
date: ""
---
'

  main_title <- sub("<insert-title>", title, template_title, fixed=TRUE)

  specs <- vector("list", length(x))

  for(i in seq_along(x)) {
    spec <- load_spec(x[[i]]$file)
    specs[[i]] <- md_outline(spec, head = NULL)
  }

  if(file.exists(output)) file.remove(output)

  cat(file = output, main_title, "\n")

  contents_header <- paste0("# Contents")

  cat(file = output, contents_header, "\n", append = TRUE)

  contents <- vector("list", length(x))
  for(i in seq_along(contents)) {
    link <- gsub("[[:punct:]]", "-", x[[i]]$name)
    contents[[i]] <- paste0("  - [__", basename(x[[i]]$source), "__](#",link,")")
    contents[[i]] <- paste0(contents[[i]], " ", x[[i]]$description)
    cat(file = output, contents[[i]], "\n\n", append = TRUE)
  }
  contents <- c(contents_header,contents)
  cat(file = output, "\n", append = TRUE)


  for(i in seq_along(specs)) {
    header <- paste0("# Dataset: ", x[[i]]$name)
    cat(file = output, header, "\n", append = TRUE)
    file_header <- paste0("### ", x[[i]]$description)
    cat(file = output, file_header, "\n", append = TRUE)
    cat(file = output, specs[[i]], "\n", sep = "\n", append = TRUE)
  }

  rmarkdown::render(input = output, output_format = "pdf_document", ...)
}

