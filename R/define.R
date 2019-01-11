
call_format_fun <- function(yamlfile,
                            format = c("x_table","pander_table", "md_outline")) {
  format <- match.arg(format)
  format_fun <- get(format, mode = "function")
  spec <- load_spec(yamlfile)
  format_fun(spec, head = NULL)
}

##' Render a document from one or more specification objects
##' 
##' @param x a spec or project object
##' @param type the document type
##' @param ... passed to [render_define] or [render_fda_define]
##' @export
ys_document <- function(x, type = c("regulatory", "working"), ...) {
  type <- match.arg(type)
  if(type=="regulatory") return(render_fda_define(x,...))
  render_define(x,...)
}

##' Render a define.pdf document
##' 
##' `render_spec` generates a define document from a single specification object
##' or file name.
##'
##' @param x a \code{yproj} object or project specification file name
##' @param stem used to name the output file
##' @param format the name of a function that will genrate code formatting
##' the data specification information
##' @param output_format passed to \code{rmarkdown::render}
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir directory where \code{rmarkdown} should build the
##' document
##' @param title used in yaml front matter
##' @param author used in yaml front matter
##' @param toc used in yaml front matter
##' @param number_sections used in yaml front matter
##' @param date used in yaml front matter
##' @param dots passed to object converter
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
##' \dontrun{
##'   render_define(file)
##' }
##' @export
render_define <- function(x, ...) {
  UseMethod("render_define")  
}

##' @rdname render_define
##' @export
render_define.yproj <- function(x, 
                                stem = "define",
                                format = c("x_table","pander_table", "md_outline"),
                                output_format = "pdf_document",
                                output_dir = getwd(),
                                build_dir = tempdir(),
                                title = "Data Specification",
                                author = "MetrumRG",
                                toc = "yes",
                                number_sections = "yes",
                                date = format(Sys.time()),...) {
  
  if(missing(toc) & length(x)==1) toc <- "no"
  if(missing(number_sections) & length(x)==1) number_sections <- "no"
  
  meta <- get_meta(x)
  
  sponsor <- "[::sponsor::]]"
  if(.has("sponsor", meta)) {
    sponsor <- meta[["sponsor"]]  
  }
  
  projectnumber <- "[::projectnumber::]"
  if(.has("projectnumber", meta)) {
    projectnumber <- meta[["projectnumber"]] 
  }
  
  sponsor <- db_quote(sponsor)
  projectnumber <- db_quote(projectnumber)
  
  proj <- meta[["proj_file"]]
  
  yamlfile <- normalizePath(proj)
  output_dir <- normalizePath(output_dir)
  build_dir <- normalizePath(build_dir)
  copy_back <- FALSE
  
  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    copy_back <- TRUE
    on.exit(setwd(cwd))
  }
  
  format <- match.arg(format)
  
  file <- paste0(stem, ".Rmd")
  
  rmd <- system.file("rmd", "define.Rmd", package = "yspec")
  
  txt <- paste0(readLines(rmd), collapse = "\n")
  
  txt <- glue::glue(txt, .open = "<", .close = ">")
  
  writeLines(txt,file)
  
  ans <- rmarkdown::render(file, output_format = output_format, ...)
  
  if(copy_back) file.copy(ans, output_dir, overwrite = TRUE)
  
  return(invisible(ans))
}

##' @rdname render_define
##' @export
render_define.character <- function(x,...,dots = list()) {
  proj <- do.call(load_spec_proj, c(list(x),dots))
  render_define(proj, ...)
}

##' @rdname render_define
##' @export
render_define.yspec <- function(...) {
  render_spec.yspec(...)
}

##' @rdname render_define
##' @export
render_spec <- function(x, ...) UseMethod("render_spec")

##' @rdname render_define
##' @export
render_spec.character <- function(x, stem = basename(x),...,dots = list()) {
  proj <- do.call(ys_project_file, c(list(x),dots))
  render_define(proj,stem = stem, ...)
}

##' @rdname render_define
##' @export
render_spec.yspec <- function(x, stem = get_meta(x)[["name"]], ..., dots = list()) {
  proj <- do.call(as_proj_spec, c(list(x),dots))
  render_define(proj, stem = stem, ...)
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
