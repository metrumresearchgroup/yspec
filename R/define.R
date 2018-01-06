
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


md_header <- function(data_file,...) {
  c("---",
    "title: ''",
    "author: ''",
    "date: ''",
    "fontsize: 12pt",
    "---\n",
    "## Data source",
    paste0("  * Data Set: ", backticks(data_file)),
    paste0("  * Date: ", Sys.Date(), "\n"),
    "## Data definitions\n")
}


md_outline <- function(x,data_file, head = TRUE,...) {

  if(!is.null(head)) head <- md_header(data_file,...)

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
##' @param data_file the data file the spec is describing
##' @param format function defining how to render the object
##' @param title used for yaml front matter
##' @param author used for yaml front matter
##' @param date used for yaml front matter
##' @param output_format passed to \code{rmarkdown::render}
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir where to build the document
##' @param ... passed to \code{rmarkdown::render}
##'
##' @export
render_spec <- function(x,
                        stem,
                        data_file = data_stem(x),
                        format = c("pander_table","md_outline"),
                        title  = "Data Specification",
                        author = NULL,
                        date = format(Sys.time()),
                        output_format="html_document",
                        output_dir = getwd(),
                        build_dir = tempdir(),...) {


  format <- match.arg(format)

  format_fun <- get(format, mode="function")

  file <- file.path(build_dir,paste0(stem, ".Rmd"))

  if(file.exists(file)) file.remove(file)

  txt <- capture.output(
    cat(format_fun(x, data_file = data_file,...),sep = "\n")
  )

  assert_that(is.character(title))

  header <- as_front_matter(title, author, date)

  cat(header, file = file, sep = "\n")

  cat("\n", file = file, append = TRUE)

  cat(txt, file=file, sep="\n", append = TRUE)

  rmarkdown::render(file, output_format=output_format,
                    output_dir=output_dir, ...)
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
##' @param file a project spec file loaded via \code{\link{load_spec_proj}}
##' @param output character stem to create a name for the output file
##' @param output_format passed to \code{rmarkdown::render}
##' @param output_dir passed to \code{rmarkdown::render}
##' @param title used for yaml front matter
##' @param author used for yaml front matter
##' @param date used for yaml front matter
##' @param ... passed to \code{rmarkdown::render}
##'
##' @details
##' \code{output} should not include a file extension, just
##' the file stem.
##'
##' @export
render_define <- function(file,
                          output = "define",
                          output_format = "html_document",
                          output_dir = getwd(),
                          title = "Data Specification",
                          author = "",
                          date = "",
                          ...) {


  .dir <- tempdir()
  output <- file.path(.dir, paste0(output, ".md"))
  x <- load_spec_proj(file)
  files <- names(x)
  n_files <- length(x)
  template_title <- '---
title: {title}
author: {author}
date: {date}
output:
  html_document:
    number_sections: true
    toc_depth: 1
    toc: true
---
'
  main_title <- glue::glue(template_title)

  specs <- vector("list", length(x))

  outlines <- map(x, function(.x) {
    spec <- load_spec(.x[["spec_file"]])
    md_outline(spec, head = NULL)
  })

  if(file.exists(output)) {
    file.remove(output)
  }

  cat(file = output, main_title, "\n")

  cat(file = output, "\n", append = TRUE)

  walk2(x, outlines, function(x,y) {
    header <- paste0("# ", x[["description"]], " ", parens(x[["name"]]))
    cat(file = output, header, "\n", append = TRUE)
    cat(file = output, y, "\n", sep = "\n", append = TRUE)
  })

  rmarkdown::render(input = output, output_format = output_format,
                    output_dir = output_dir, ...)
}

