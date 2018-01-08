
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

# Functions for generating spec tables for a
# single data set
as_fda_table_ <- function(x) {
  variable <- x[["col"]]
  label <- long(x, default = x[["col"]])
  if(.has("unit", x)) {
    label <- paste0(label, " (unit: ", x$unit, ")")
  }

  data_frame(VARIABLE = x[["col"]],
             LABEL = label,
             TYPE = type(x, " "),
             CODES = pack_codes(x))
}

as_fda_table <- function(x) {
  map_df(x, as_fda_table_)
}

add.to.row <- list(pos = list(0), command = NULL)
command <- paste0("\\hline\n\\endhead\n",
                  "\\hline\n",
                  "\\multicolumn{4}{l}",
                  "{\\footnotesize Continued on next page}\n",
                  "\\endfoot\n",
                  "\\endlastfoot\n")

add.to.row$command <- command

lengths <- c(0, 0.75, 1.85, 0.6, 1.8)
align <- paste0("p{",lengths,"in}|")
align[2] <- paste0("|", align[2])

##' Generate a table for FDA define.pdf document
##'
##' @param x a yspec object
##' @param file the full path to yaml specification file
##'
##' @return Character vector of latex code for
##' the content of an FDA define.pdf document.  It
##' includes a table of contents as well as data spec
##' tables for each dataset for a project.  Use
##' \code{\link{print_fda_define}} to print the
##' output in the context of an Rmarkdown document.
##'
##' @seealso \code{\link{print_fda_define}}
##'
##' @export
fda_table <- function(x) {
  if(!is_yspec(x)) {
    .stop("x is not a yspec object")
  }
  x <- as_fda_table(x)
  xx <- xtable(x, align = align)
  capture.output(
    print(xx, hline.after=c(-1,0,seq_len(nrow(xx)-1)),
          add.to.row = add.to.row, comment = FALSE,
          include.rownames = FALSE, table.placement = "H",
          tabular.environment = "longtable", floating = FALSE)
  )
}

##' @rdname fda_table
##' @export
fda_table_file <- function(file) {
  x <- load_spec(file)
  fda_table(x)
}


##' Print a define document suitable for FDA submission
##'
##' @param file full path to define yaml file
##' @param title used in yaml front matter
##' @param ... arguments to \code{fda_define}
##'
##' @details
##' Use \code{print_fda_define} to generate the output in the context
##' of an Rmarkdown document.  Use \code{fda_define} to generate the
##' document as a character vector.
##'
##' @export
fda_define <- function(file, title = "Datasets") {

  x <- load_spec_proj(file)

  main <- paste0("# ", title)

  contents <- fda_content_table(x)

  specs <- map(x, function(this) {
    title <- paste0(this$description, " (`", this$data_file, "`)")
    header <- paste0("## ", title, " \\label{", this$name,"}")
    c(header, "\\noindent", " ", "  ", fda_table_file(this$spec_file))
  })
  c(main, contents, flatten_chr(specs))
}

##' @rdname fda_define
##' @export
print_fda_define <- function(...) {
  writeLines(fda_define(...))
}


##' Print a table of contents for FDA define document
##'
##' @param x a spec define object
##' @param file the full path to a yaml specification file
##' @seealso \code{\link{load_spec_proj}}
##' @export
fda_content_table <- function(x) {
  if(!is_yproj(x)) {
    .stop("x is not a project specification object")
  }
  contents <- fda_content_rows(x)
  kable(contents,
        format = "latex",
        align = c("|p{2.85in}", "p{2.55in}|"),
        escape = FALSE)
}

##' @rdname fda_content_table
##' @export
fda_content_table_file <- function(file) {
  fda_content_table(load_spec_proj(file))
}

fda_content_rows <- function(x) {
  map_df(x, function(.x) {
    loc <- fda_content_ref(.x[["name"]], .x[["data_file"]])
    data_frame(Description  = .x$description,
               Location = loc)
  })
}

fda_content_ref <- function(name, data_file) {
  data_file <- gsub("_", "\\\\_", data_file)
  paste0("\\hyperref[",name,"]{", data_file, "}")
}



##' Render a define document for sending to FDA
##'
##' @param x a yaml specification file name or a yproj object
##' @param title a title for the document
##' @param date the document date
##' @param author the document author
##' @param output_dir passed to \code{rmarkdown::render}
##' @param build_dir directory where rmarkdown will build the document
##' @param ... passed to \code{rmarkdown::render}
##'
##' @export
render_fda_define <- function(x, ... ) {
  UseMethod("render_fda_define")
}

##' @rdname render_fda_define
##' @export
render_fda_define.yproj <- function(x, ...) {
  m <- get_meta(x)
  project_file_name <- m$yml_file
  assert_that(is.character(project_file_name))
  render_fda_define(project_file_name, ...)
}

##' @rdname render_fda_define
##' @export
render_fda_define.character <- function(x,
                                        title = "Data Definitions",
                                        date = format(Sys.time()),
                                        author = "MetrumRG Staff Scientist",
                                        output_dir = getwd(),
                                        build_dir = tempdir(),
                                        ...) {

  yamlfile <- normalizePath(x)

  output_dir <- normalizePath(output_dir)

  cwd <- normalizePath(getwd())
  if(cwd != build_dir) {
    setwd(build_dir)
    on.exit(setwd(cwd))
  }

  proj <- load_spec_proj(yamlfile)

  m <- get_meta(proj)

  if(!is.character(m[["sponsor"]])) {
    .stop("sponsor field is required in SETUP__")
  }
  sponsor <- m[["sponsor"]]

  if(!is.character(m[["projectnumber"]])) {
    .stop("projectnumber field is required in SETUP__")
  }
  projectnumber <- m[["projectnumber"]]

  rmd <- system.file("rmd", "fdadefine.Rmd", package = "yspec")

  txt <- paste0(readLines(rmd),collapse = "\n")

  txt <- glue(txt, .open = "<", .close = ">")

  writeLines(txt,"define.Rmd")


  ans <- rmarkdown::render("define.Rmd", output_dir = output_dir, ...)
  return(invisible(ans))

}
