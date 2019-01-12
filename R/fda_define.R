fda_label <- function(x) {
  if(.no("long", x)) {
    return(x[["short"]]) 
  }
  return(x[["long"]])
}

pack_codes <- function(x) {
  if(.no("values",x)) return("")
  if(.no("decode",x)) {
    ans <- paste(x[["values"]], collapse = ', ')
    ans <- paste0("values: ", ans)
    return(ans)
  }
  ans <- paste0(x[["values"]], " = ", x[["decode"]])
  paste(ans, collapse = ", ")
}

as_fda_table_row <- function(x) {
  variable <- x[["col"]]
  label <- long(x, default = x[["short"]])
  if(.has("unit", x)) {
    label <- paste0(label, " (unit: ", x$unit, ")")
  }
  
  data_frame(
    VARIABLE = x[["col"]],
    LABEL = label,
    TYPE = type(x, " "),
    CODES = pack_codes(x)
  )
}

as_fda_table <- function(x) {
  map_df(x, as_fda_table_row)
}

add.to.row <- list(pos = list(0), command = NULL)
command__ <- paste0(
  "\\hline\n\\endhead\n",
  "\\hline\n",
  "\\multicolumn{4}{l}",
  "{\\footnotesize Continued on next page}\n",
  "\\endfoot\n",
  "\\endlastfoot\n"
)
add.to.row$command <- command__

##' Generate a table for FDA define.pdf document
##'
##' @param x a yspec object
##' @param file the full path to yaml specification file
##'
##' @return Character vector of latex code for
##' the content of an FDA define.pdf document.  It
##' includes a table of contents as well as data spec
##' tables for each dataset for a project.
##'
##' @examples
##' spec <- load_spec_ex()
##' spec
##' fda_table(spec)
##'
##' @export
fda_table <- function(x) {
  if(!is_yspec(x)) {
    .stop("x is not a yspec object")
  }
  x <- as_fda_table(x)
  lengths <- c(0, 0.75, 1.85, 0.6, 1.8)
  align <- paste0("p{",lengths,"in}|")
  align[2] <- paste0("|", align[2])
  xx <- xtable(x, align = align)
  capture.output(
    print(
      xx, hline.after=c(-1,0,seq_len(nrow(xx)-1)),
      add.to.row = add.to.row, comment = FALSE,
      include.rownames = FALSE, table.placement = "H",
      tabular.environment = "longtable", floating = FALSE
    )
  )
}

##' Print a table of contents for FDA define document
##'
##' @param x a spec define object
##' @param file the full path to a yaml specification file
##' @inheritParams fda_define 
##' @seealso \code{\link{load_spec_proj}}
##'
##' @examples
##' proj <- file_proj_ex()
##'
##' spec <- load_spec_proj(proj)
##'
##' fda_content_table(spec)
##'
##' @export
fda_content_table <- function(x, ext=".xpt", loc=".") {
  if(!is_yproj(x)) {
    .stop("x is not a project specification object")
  }
  loc <- gsub("/$", "", loc)
  contents <- map_df(x, fda_content_table_row, ext=ext, loc=loc)
  kable(
    contents,
    format = "latex",
    align = c("|p{2.85in}", "p{2.55in}|"),
    escape = FALSE
  )
}

##' @rdname fda_table
##' @export
fda_table_file <- function(file) {
  x <- load_spec(file)
  fda_table(x)
}

# ##' @rdname fda_content_table
# ##' @export
# fda_content_table_file <- function(file,...) {
#   fda_content_table(load_spec_proj(file),...)
# }

fda_content_table_row <- function(.x, ext, loc) {
  data_file <- paste0(.x[["data_stem"]], ext)
  desc <- fda_content_table_ref(.x[["name"]],.x[["description"]])
  location <-  fda_content_table_loc(data_file,loc)
  data_frame(Description  = desc, Location = location)
}

fda_content_table_ref <- function(name, desc) {
  paste0("\\hyperref[",name,"]{", desc, "}")
}

fda_content_table_loc <- function(data_file,loc) {
  loc_display <- gsub("_", "\\\\_", data_file)
  paste0("\\href{run:",loc,"/",data_file, "}{",loc_display,"}")
}

##' Generate content for FDA define document
##'
##' @param file full path to define yaml file
##' @param title used in yaml front matter
##' @param ext data set file extension to include; this should only 
##' be changed from default value of ".xpt" for testing puroses
##' @param loc location to use for data set files; this should 
##' only be changed from default value of "." for testing purposes
##' @param ... arguments passed to rendering functions; see
##' details
##'
##' @return
##' A character vector of in markdown format.  Wrap
##' \code{fda_define} in \code{\link{writeLines}} and
##' render \code{asis} in an Rmarkdown document.
##'
##' @examples
##' proj <- file_proj_ex()
##'
##' cat(readLines(proj), sep = "\n")
##'
##' \dontrun{
##'   fda_define(proj)
##' }
##' @seealso \code{\link{fda_table}}
##' @export
fda_define <- function(file, title="Datasets", ext=".xpt", loc=".",...) {
  
  x <- load_spec_proj(file)
  
  main <- paste0("# ", title)

  contents <- fda_content_table(x, ext=ext, loc=loc, ...)
  
  specs <- map(x, function(this) {
    title <- paste0(this$description, " (`", this$data_file, "`)")
    header <- paste0("## ", title, " \\label{", this$name,"}")
    c(header, "\\noindent", " ", "  ", fda_table_file(this$spec_file))
  })
  c(main, contents, flatten_chr(specs))
}

##' Render a define document conforming to FDA standards
##'
##' @param x a yaml specification file name or a yproj object
##' @param stem used to name the output document
##' @param title a title for the document
##' @param date the document date
##' @param author the document author
##' @param format function to generate the define text
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir directory where rmarkdown will build the document
##' @param dots named list of arguments passed to object converter function
##' @inheritParams fda_define
##' @param ... passed to \code{rmarkdown::render}
##'
##' @examples
##' proj_file <- file_proj_ex()
##'
##' proj_file
##' 
##' \dontrun{
##'   render_fda_define(proj_file)
##' }
##' 
##' @export
render_fda_define <- function(x, ... ) {
  UseMethod("render_fda_define")
}

##' @rdname render_fda_define
##' @export
render_fda_define.yproj <- function(x, 
                                    stem = "define",
                                    title = "Data Definitions",
                                    date = format(Sys.time()),
                                    author = "MetrumRG Staff Scientist",
                                    format = "fda_define",
                                    output_dir = getwd(),
                                    build_dir = tempdir(),
                                    ext = ".xpt", loc = '.', 
                                    ...) {
  
  output_dir <- normalizePath(output_dir)
  build_dir <- normalizePath(build_dir)
  cwd <- normalizePath(getwd())
  copy_back <- FALSE
  if(cwd != build_dir) {
    copy_back <- TRUE
    setwd(build_dir)
    on.exit(setwd(cwd))
  }
  
  meta <- get_meta(x)
  
  yamlfile <- meta[["proj_file"]]
  
  if(!is.character(meta[["sponsor"]])) {
    .stop("sponsor field is required in SETUP__")
  }
  sponsor <- meta[["sponsor"]]
  
  if(!is.character(meta[["projectnumber"]])) {
    .stop("projectnumber field is required in SETUP__")
  }
  projectnumber <- meta[["projectnumber"]]
  
  sponsor <- db_quote(sponsor)
  projectnumber <- db_quote(projectnumber)
  
  if(is.character(format)) {
    format_fun <- get(format, mode = "function")
  } else {
    format_fun <- format
  }
  
  ys_regulatory_markup_ <- basename(tempfile(fileext="aeiou"))
  env <- new.env()
  env[[ys_regulatory_markup_]] <- format_fun(yamlfile, ext=ext, loc=loc)
  
  rmd <- system.file("rmd", "fdadefine.Rmd", package = "yspec")
  
  txt <- paste0(readLines(rmd),collapse = "\n")
  
  txt <- glue(txt, .open = "<", .close = ">")
  
  .file <- paste0(stem, ".Rmd")
  
  writeLines(txt,.file)
  
  ans <- rmarkdown::render(.file, envir = env, ...)
  
  if(copy_back) file.copy(ans, output_dir, overwrite = TRUE)
  
  return(invisible(ans))
  
}

##' @rdname render_fda_define
##' @export
render_fda_define.character <- function(x,...,dots = list()) {
  proj <- do.call(ys_project_file, c(list(x),dots))
  render_fda_define.yproj(proj, ...)
}

##' @rdname render_fda_define
##' @export
render_fda_define.yspec <- function(x, ..., dots = list()) {
  dots <- c(list(x),dots)
  proj <- do.call(ys_project, dots)
  render_fda_define.yproj(proj,...)  
}




