
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

lengths <- c(0, 0.75, 1.85, 0.75, 2)
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
##' @param main Title for the main heading
##' @param ... passed to \code{\link{print_fda_define}}
##'
##' @details
##' Use \code{print_fda_define} to generate the output in the context
##' of an Rmarkdown document.  Use \code{fda_define} to generate the
##' document as a character vector.
##'
##' @export
print_fda_define <- function(file, main = "Datasets") {

  x <- load_spec_proj(file)

  writeLines(paste0("# ", main))

  writeLines(fda_content_table(x))

  walk(x, function(this) {
    title <- paste0(this$description, " (`", this$source, "`)")
    header <- paste0("## ", title, " \\label{", this$name,"}")
    writeLines(header)
    writeLines(fda_table_file(this$file))
  })
}

##' @rdname print_fda_define
##' @export
fda_define <- function(...) {
  capture.output(print_fda_define(...))
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
        align = c("|p{2.85in}", "p{2.75in}|"),
        escape = FALSE)
}

##' @rdname fda_content_table
##' @export
fda_content_table_file <- function(file) {
  fda_content_table(load_spec_proj(file))
}

fda_content_rows <- function(x) {
  map_df(x, function(.x) {
    loc <- fda_content_ref(.x[["name"]], .x[["source"]])
    data_frame(Description  = .x$description,
               Location = loc)
  })
}

fda_content_ref <- function(name, source) {
  source <- gsub("_", "\\\\_", source)
  paste0("\\hyperref[",name,"]{", source, "}")
}
