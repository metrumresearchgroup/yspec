


call_format_fun <- function(yamlfile,
                            format = c("x_table","pander_table", "md_outline")) {
  format <- match.arg(format)
  format_fun <- get(format, mode = "function")
  spec <- load_spec(yamlfile)
  format_fun(spec, head = NULL)
}

##' Render a data specification file
##'
##' @param x a data specification file name or a yspec object
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
##' @examples
##'
##' file <- file_spec_ex()
##' render_spec(file)
##'
##' spec <- load_spec_ex()
##' render_spec(spec)
##'
##' @export
render_spec <- function(x, ...) UseMethod("render_spec")

##' @rdname render_spec
##' @export
render_spec.character <- function(x,
                                  stem = basename(x),
                                  format = c("x_table","pander_table","md_outline"),
                                  title  = "Data Specification",
                                  author =  "MetrumRG",
                                  date = format(Sys.time()),
                                  output_format="pdf_document",
                                  output_dir = getwd(),
                                  build_dir = tempdir(), ...) {

  yamlfile <- normalizePath(x)

  output_dir <- normalizePath(output_dir)

  build_dir <- normalizePath(build_dir)

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

  ans <- rmarkdown::render(file, output_format = output_format,
                           output_dir = output_dir, ...)

  return(invisible(ans))
}

##' @rdname render_spec
##' @export
render_spec.yspec <- function(x, ...) {
  render_spec.character(get_meta(x)[["yml_file"]], ...)
}

##' Generate code for a generic define document
##'
##' @param yamlfile a project file name
##' @param format a function or the name of a function to format the spec
##' contents
##' @export
define_for_rmd <- function(yamlfile, format) {

  if(is.character(format)) {
    format_fun <- get(format, mode = "function")
  } else {
    format_fun <- format
  }

  assert_that(is.function(format_fun))

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

  specs
}

##' Render a define.pdf document
##'
##' @param file a project spec file loaded via \code{\link{load_spec_proj}}
##' @param stem used to name the output file
##' @param format the name of a function that will genrate code formatting
##' the data specification information
##' @param output_format passed to \code{rmarkdown::render}
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir directory where \code{rmarkdown} should build the
##' document
##' @param title used in yaml front matter
##' @param author used in yaml front matter
##' @param sponsor used in yaml front matter
##' @param projectnumber used in yaml front matter
##' @param toc used in yaml front matter
##' @param date used in yaml front matter
##' @param ... passed to \code{rmarkdown::render}
##'
##' @details
##' \code{stem} should not include a file extension, just
##' the file stem.
##'
##' @examples
##'
##' file <- file_proj_ex()
##'
##' file
##'
##' render_define(file)
##'
##' @export
render_define <- function(file,
                          stem = basename(file),
                          format = c("x_table","pander_table", "md_outline"),
                          output_format = "pdf_document",
                          output_dir = getwd(),
                          build_dir = tempdir(),
                          title = "Data Specification",
                          author = "MetrumRG",
                          sponsor = "",
                          projectnumber = "",
                          toc = "yes",
                          date = format(Sys.time()),
                          ...) {

  yamlfile <- normalizePath(file)
  output_dir <- normalizePath(output_dir)
  build_dir <- normalizePath(build_dir)
  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }

  format <- match.arg(format)

  file <- paste0(stem, ".Rmd")

  rmd <- system.file("rmd", "define.Rmd", package = "yspec")

  txt <- paste0(readLines(rmd), collapse = "\n")

  txt <- glue::glue(txt, .open = "<", .close = ">")

  writeLines(txt,file)

  return(invisible(rmarkdown::render(file, output_format = output_format,
                                     output_dir = output_dir, ...)))
}

