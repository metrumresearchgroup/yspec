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
  label <- label(x, default = "short")
  if(.has("unit", x)) {
    label <- paste0(label, " (unit: ", x$unit, ")")
  }
  tibble(
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

add.to.row_long <- list(pos = list(0), command = NULL)
command__ <- paste0(
  "\\hline\n\\endhead\n",
  "\\hline\n",
  "\\multicolumn{3}{l}",
  "{\\footnotesize Continued on next page}\n",
  "\\endfoot\n",
  "\\endlastfoot\n"
)
add.to.row_long$command <- command__



##' Generate a table for FDA define.pdf document
##'
##' @param x a yspec object
##' @param file the full path to yaml specification file
##'
##' @return Character vector of latex code for the content of an FDA 
##' `define.pdf` document.  It includes a table of contents as well as data spec
##' tables for each dataset for a project.
##'
##' @examples
##' spec <- load_spec_ex()
##' spec
##' fda_table(spec)
##' 
##' @md
##' @export
fda_table <- function(x) {
  if(!is_yspec(x)) {
    .stop("x is not a yspec object")
  }
  tab <- as_fda_table(x)
  lengths <- c(0, 0.75, 2.1, 0.6, 2.2)
  align <- paste0(">{\\raggedright\\arraybackslash}p{",lengths,"in}|")
  align[2] <- paste0("|", align[2])
  xtab <- xtable(tab, align = align)
  ans <- capture.output(
    print(
      xtab, hline.after=c(-1,0,seq_len(nrow(xtab)-1)),
      add.to.row = add.to.row, comment = FALSE,
      include.rownames = FALSE, table.placement = "H",
      tabular.environment = "longtable", floating = FALSE, 
      sanitize.text.function = getOption("ys.sanitize", ys_sanitize)
    )
  )
  glu <- get_meta(x)[["glue"]]
  if(is.list(glu)) {
    ans <- sapply(ans, glue, .envir = glu, .open = .glopen, .close = .glclose)
  }
  unname(ans)
}

##' Print a table of contents for FDA define document
##'
##' @param x a spec define object
##' @inheritParams fda_define 
##' 
##' @seealso [load_spec_proj]
##'
##' @examples
##' proj <- file_proj_ex()
##'
##' spec <- load_spec_proj(proj)
##'
##' fda_content_table(spec)
##' @md
##' @export
fda_content_table <- function(x, ext=".xpt", loc=".") {
  if(!is_yproj(x)) {
    .stop("x is not a project specification object")
  }
  loc <- gsub("/$", "", loc)
  contents <- map_df(x, fda_content_table_row, ext=ext, loc=loc)
  ans <- kable(
    contents,
    format = "latex",
    align = c("|>{\\raggedright\\arraybackslash}p{2.85in}", ">{\\raggedright\\arraybackslash}p{3.15in}|"),
    escape = FALSE, longtable=TRUE
  )
  ans
}

##' @rdname fda_table
##' @export
fda_table_file <- function(file) {
  x <- load_spec(file)
  fda_table(x)
}

fda_content_table_row <- function(.x, ext, loc) {
  data_file <- paste0(.x[["data_stem"]], ext)
  desc <- fda_content_table_ref(.x[["name"]],.x[["description"]])
  location <-  fda_content_table_loc(data_file,loc)
  tibble(Description  = desc, Location = location)
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
##' be changed from default value of ".xpt" for testing purposes
##' @param loc location to use for data set files; this should 
##' only be changed from default value of "." for testing purposes
##' @param ... arguments passed to rendering functions; see
##' details
##'
##' @return
##' A character vector of in markdown format.  Wrap
##' `fda_define` in [writeLines()] and
##' render `asis` in an Rmarkdown document.
##'
##' @examples
##' proj <- file_proj_ex()
##'
##' cat(readLines(proj), sep = "\n")
##'
##' \dontrun{
##'   fda_define(proj)
##' }
##' @seealso [fda_table]
##' @md
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

##' Render a define.pdf document conforming to FDA standards
##'
##' @param x a yaml specification file name or a yproj object
##' @param stem used to name the output document
##' @param title a title for the document
##' @param date the document date
##' @param author the document author
##' @param format function to generate the define text
##' @param dots named list of arguments passed to object converter function
##' @param build_dir directory where the document is to be built
##' @inheritParams fda_define
##' @inheritParams rmarkdown::render 
##' @param ... passed to [rmarkdown::render()]
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
##' @section latex requirements:
##' 
##' For all document types, the following `latex` packages are required: 
##' 
##' 1. `array`
##' 1. `longtable`
##' 1. `booktabs`
##' 1. `fontenc`
##' 1. `mathdesign`
##' 
##' Make sure these packages are installed and available when trying to render a document.
##' 
##' 
##' @md
##' @export
render_fda_define  <- function(x, ... ) {
  UseMethod("render_fda_define")
}

##' @rdname render_fda_define
##' @export
render_fda_define.yproj <- function(x, 
                                    stem = "define",
                                    title = "Data Definitions",
                                    date = as.character(Sys.Date()),
                                    author = "",
                                    format = "fda_define",
                                    output_dir = getwd(),
                                    build_dir = definetemplate(),
                                    ext = ".xpt", loc = '.', 
                                    ...) {
  
  output_dir <- normalPath(output_dir)
  build_dir <- normalPath(build_dir)
  cwd <- normalPath(getwd())
  copy_back <- !identical(build_dir, output_dir)
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }
  
  meta <- get_meta(x)
  
  yamlfile <- meta[["spec_file"]]
  
  if(!is.character(meta[["sponsor"]])) {
    .stop("sponsor field is required in YPROJ__")
  }
  sponsor <- meta[["sponsor"]]
  
  if(!is.character(meta[["projectnumber"]])) {
    .stop("projectnumber field is required in YPROJ__")
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




