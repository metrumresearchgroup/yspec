
call_format_fun <- function(yamlfile,
                            format = c("x_table", "x_table_long","pander_table", "md_outline")) {
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
##' @param ... passed to [render_define] or [render_fda_define]; it is important
##' to review these help topics to see what other aspects of the document 
##' can be specified; see also `details` here.
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
##' Because `...` are passed to [render_define] and [render_fda_define], 
##' it is important to review arguments to those functions as well.  Specifically, 
##' please note that the document **title**, **author**, and **date** can 
##' be set, along with the name of the output document, the working document 
##' build directory, and several other aspects of the document can be set
##' in the call to [ys_document]. 
##' 
##' @examples
##' 
##' \dontrun{
##'   ys_document(ys_help$spec())
##'   ys_document(ys_help$spec(), type = "regulatory")
##'   ys_document(ys_help$spec(), type = "regulatory", build_dir = mrgtemplate())
##' }
##' @seealso [render_define], [render_fda_define]
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
##' `render_spec` is an alias to `render_define`.  See details.
##'
##' @param x a `yproj` object or project specification file name
##' @param stem used to name the output file
##' @param format the name of a function that will generate code formatting
##' the data specification information
##' @param output_format passed to [rmarkdown::render]
##' @param output_dir passed to [rmarkdown::render]
##' @param build_dir directory where `rmarkdown` should build the
##' document
##' @param title used in yaml front matter
##' @param author used in yaml front matter
##' @param toc used in yaml front matter
##' @param number_sections used in yaml front matter
##' @param rmd_template full path to rmarkdown file to be used to template the 
##' data specification document
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
##' file <- ys_help$file()
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
                                format = c("x_table","x_table_long","pander_table", "md_outline"),
                                output_format = "pdf_document",
                                output_dir = getwd(),
                                build_dir = tempdir(),
                                title = "Data Specification",
                                author = "MetrumRG",
                                toc = "yes",
                                number_sections = "yes",
                                rmd_template = NULL,
                                date = base::format(Sys.time()),...) {
  
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
  
  yamlfile <- normalPath(proj)
  output_dir <- normalPath(output_dir)
  build_dir <- normalPath(build_dir)
  copy_back <- !identical(build_dir,output_dir)
  
  cwd <- normalPath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }

  ys_working_markup_ <- basename(tempfile(fileext="aeiou"))
  
  env <- new.env()
  env[[ys_working_markup_]] <- define_for_rmd(yamlfile,format,x,meta) 
  
  file <- normalPath(paste0(stem, ".Rmd"),mustWork=FALSE)
  
  if(is.null(rmd_template)) {
    rmd_template <- system.file("rmd", "define.Rmd", package = "yspec")
  } 
  
  txt <- paste0(readLines(rmd_template), collapse = "\n")
  
  txt <- glue::glue(txt, .open = "<", .close = ">")
  
  writeLines(txt,file)
  
  ans <- rmarkdown::render(
    file, 
    output_format = output_format, 
    envir=env,
    ...
  )
  
  if(copy_back) file.copy(normalPath(ans), output_dir, overwrite = TRUE)
  
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
##' @param x a project file name
##' @param form_ a function or the name of a function to format the spec
##' contents
##' @param proj a project object from which to render
##' @param meta meta data list 
##' @keywords internal
##' @md
##' @export
define_for_rmd <- function(x,form_,proj=NULL,meta=NULL) {
  
  if(is.character(form_)) {
    format_fun <- get(form_, mode = "function")
  } else {
    format_fun <- form_
  }
  
  assertthat::assert_that(is.function(format_fun))
  
  environment(format_fun) <- parent.frame()
  
  if(is.null(proj)) {
    proj <- load_spec_proj(x)  
  }
  if(is.null(meta)) {
    meta <- get_meta(proj)  
  }
  
  if(.has("data", meta) & getOption("yspec.use.kept.data",FALSE)) {
    warning(
      "using spec data found in yproject object, not from the source yaml file.",
      call.=FALSE
    )
    specs <- meta[["data"]]  
  } else {
    file_names <- map(proj, "spec_file")
    specs <- map(file_names,load_spec)
  }
  
  tex <- imap(proj, .f = function(xi,.name) {
    description <- proj[[.name]][["description"]]
    sp <- format_fun(specs[[.name]])
    c(paste0("# ", .name),
      "",
      "__Description__: ",
      description,"",
      sp, " ")
  })
  
  tex <- flatten_chr(tex)
  
  tex
}

#' Render an arbitrary spec of project
#' 
#' 
#' @inheritParams ys_document
#' 
ys_render <- function(...) {
  ys_document(...)
} 
