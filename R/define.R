
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
    "fontsize: 16pt",
    "---\n",
    "## Data source",
    paste0("  * Data Set: ", backticks(data_file)),
    paste0("  * Date: ", Sys.Date(), "\n"),
    "## Data definitions\n")
}


md_outline <- function(x, data_file = "", head = NULL,...) {

  if(!is.null(head)) head <- md_header(data_file,...)

  txt <- lapply(x, define_col_1)

  c(head, flatten_chr(txt))

}

call_format_fun <- function(yamlfile,
                            format = c("pander_table", "md_outline")) {
  format <- match.arg(format)
  format_fun <- get(format, mode = "function")
  spec <- load_spec(yamlfile)
  format_fun(spec, head = NULL)
}


##' Render a data specification file
##'
##' @param yamlfile a data specification file name
##' @param stem for output file name
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
render_spec <- function(yamlfile,
                        stem = basename(yamlfile),
                        format = c("pander_table","md_outline"),
                        title  = "Data Specification",
                        author =  "MetrumRG",
                        date = format(Sys.time()),
                        output_format="html_document",
                        output_dir = getwd(),
                        build_dir = tempdir(),...) {

  yamlfile <- normalizePath(yamlfile)

  output_dir <- normalizePath(output_dir)

  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }

  format <- match.arg(format)

  file <- paste0(stem, ".Rmd")

  rmd <- system.file("rmd", "spec.Rmd", package = "yspec")

  txt <- paste0(readLines(rmd), collapse = "\n")

  txt <- glue::glue(txt, .open = "<", .close = ">")

  writeLines(txt,file)

  return(invisible(rmarkdown::render(file, output_format = output_format,
                                     output_dir = output_dir, ...)))
}



print_define_for_rmd <- function(yamlfile,
                                 format, ...) {
  format_fun <- get(format, mode = "function")

  proj <- load_spec_proj(yamlfile)
  specs <- imap(proj, .f = function(x,name) {
    description <- proj[[name]][["description"]]
    sp <- load_spec(x[["spec_file"]])
    sp <- format_fun(sp)
    c(paste0("# ", name),
      "",
      "__Description__: ",
      description,"",
      sp, " ")
  })
  specs <- flatten_chr(specs)
  writeLines(specs)
}


##' Render a define.pdf document
##'
##' @param file a project spec file loaded via \code{\link{load_spec_proj}}
##' @param stem used to name the output file
##' @param format the name of a function that will genrate code formatting
##' the data specification information
##' @param output_format passed to \code{rmarkdown::render}
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir directory where \code{rmarkdown} should build the document
##' @param title used in yaml front matter
##' @param author used in yaml front matter
##' @param date used in yaml front matter
##' @param ... passed to \code{rmarkdown::render}
##'
##' @details
##' \code{output} should not include a file extension, just
##' the file stem.
##'
##' @export
render_define <- function(file,
                          stem = basename(file),
                          format = c("pander_table", "md_outline"),
                          output_format = "html_document",
                          output_dir = getwd(),
                          build_dir = tempdir(),
                          title = "Data Specification",
                          author = "MetrumRG",
                          date = format(Sys.time()),
                          ...) {

  yamlfile <- normalizePath(file)
  output_dir <- normalizePath(output_dir)
  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }

  sponsor <- ""
  projectnumber <- ''

  format <- match.arg(format)

  file <- paste0(stem, ".Rmd")

  rmd <- system.file("rmd", "define.Rmd", package = "yspec")

  txt <- paste0(readLines(rmd), collapse = "\n")

  txt <- glue::glue(txt, .open = "<", .close = ">")

  writeLines(txt,file)

  return(invisible(rmarkdown::render(file, output_format = output_format,
                                     output_dir = output_dir, ...)))
}

