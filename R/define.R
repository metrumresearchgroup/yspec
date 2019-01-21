
call_format_fun <- function(yamlfile,
                            format = c("x_table","pander_table", "md_outline")) {
  format <- match.arg(format)
  format_fun <- get(format, mode = "function")
  spec <- load_spec(yamlfile)
  format_fun(spec, head = NULL)
}

##' Render a document from one or more specification objects
##' 
##' This function is a wrapper around [render_define] and [render_fda_define].
##' 
##' @param x a spec or project object
##' @param type the document type
##' @param ... passed to [render_define] or [render_fda_define]
##' 
##' @details
##' 
##' Select `type` of "working` for a working document that contains more 
##' information in a less-compact layout.  This is a good format for a modeling
##' and simulation scientist to use day-to-day and for sharing with a sponsor
##' to review.  
##' 
##' Select `type` of "regulatory" for a define document that conforms to 
##' submission requirements set by regulatory authorities (e.g. FDA).
##' 
##' Note that `x` is usually either a `yspec` object or a `yproj` object. 
##' You can also pass in the full path to a specification document and
##' yspec will guess which format it is and render accordingly.  
##' 
##' @examples
##' 
##' \dontrun{
##'   ys_document(load_spec_ex())
##'   ys_document(load_spec_ex(), type = "regulatory")
##'   ys_document(load_spec_ex(), type = "regulatory", build_dir = mrgtemplate())
##' }
##' @md
##' @export
ys_document <- function(x, type = c("working", "regulatory"), ...) {
  if(is.character(x)) {
    return(ys_document(load_spec_any(x), type = type, ...))  
  }
  type <- match.arg(type)
  if(type=="regulatory") return(render_fda_define(x,...))
  render_define(x,...)
}

##' Render a `define.pdf` document
##' 
##' `render_spec` is an alias to `render_define`.  See detils.
##'
##' @param x a `yproj` object or project specification file name
##' @param stem used to name the output file
##' @param format the name of a function that will genrate code formatting
##' the data specification information
##' @param output_format passed to [rmarkdown::render]
##' @param output_dir passed to [rmarkdown::render]
##' @param build_dir directory where `rmarkdown` should build the
##' document
##' @param title used in yaml front matter
##' @param author used in yaml front matter
##' @param toc used in yaml front matter
##' @param number_sections used in yaml front matter
##' @param date used in yaml front matter
##' @param dots passed to object converter
##' @param ... passed to [rmarkdown::render]
##' 
##'
##' @details
##' `stem` should not include a file extension, just
##' the file stem.
##' 
##' 
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
##' @md
##' @export
render_define <- function(x, ...) {
  UseMethod("render_define")  
}

##' @rdname render_define
##' @export
render_define.yproj <- function(x, 
                                stem = "define_working",
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
  
  sponsor <- "[[sponsor]]"
  if(.has("sponsor", meta)) {
    sponsor <- meta[["sponsor"]]  
  }
  
  projectnumber <- "[projectnumber]"
  if(.has("projectnumber", meta)) {
    projectnumber <- meta[["projectnumber"]] 
  }
  
  sponsor <- db_quote(sponsor)
  projectnumber <- db_quote(projectnumber)
  
  proj <- meta[["spec_file"]]
  
  yamlfile <- normalizePath(proj)
  output_dir <- normalizePath(output_dir)
  build_dir <- normalizePath(build_dir)
  copy_back <- !identical(build_dir,output_dir)
  
  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }
  
  format <- match.arg(format)
  
  ys_working_markup_ <- basename(tempfile(fileext="aeiou"))
  
  env <- new.env()
  env[[ys_working_markup_]] <- define_for_rmd(yamlfile,format)
  
  file <- normalizePath(paste0(stem, ".Rmd"))
  
  rmd <- system.file("rmd", "define.Rmd", package = "yspec")
  
  txt <- paste0(readLines(rmd), collapse = "\n")
  
  txt <- glue::glue(txt, .open = "<", .close = ">")
  
  writeLines(txt,file)
  
  ans <- rmarkdown::render(
    file, 
    output_format = output_format, 
    envir=env,
    ...
  )
  
  if(copy_back) file.copy(normalizePath(ans), output_dir, overwrite = TRUE)

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
##' This function is for internal use by [render_define].  
##'
##' @param yamlfile a project file name
##' @param format a function or the name of a function to format the spec
##' contents
##' @keywords internal
##' @md
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
