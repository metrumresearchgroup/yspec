
pack_codes <- function(x) {
  if(is.null(x[["values"]])) return("")
  if(is.null(x[["decode"]])) {
    ans <- paste(x[["values"]], collapse = ', ')
    ans <- paste0("values: ", ans)
    return(ans)
  }
  ans <- paste0(x[["decode"]], " = ", x[["values"]])
  paste(ans, collapse = ", ")
}

# Functions for generating spec tables for a
# single data set
as_fda_table_ <- function(x) {
  x1 <- x[["col"]]
  x2 <- x[["long"]]
  if(is.null(x2)) x2 <- x[["col"]]
  data_frame(VARIABLE = x1,
             LABEL = x2,
             TYPE = "numeric",
             CODES = pack_codes(x))
}

as_fda_table <- function(x) {
  bind_rows(lapply(x,as_fda_table_))
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
  if(!is_yspec(x)) stop("x is not a yspec object", call. = FALSE)
  x <- as_fda_table(x)
  xx <- xtable(x, align = align )
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

##' Load a spec define yaml document
##'
##' @param file file name
##'
##' @export
spec_define <- function(file) {
  x <- yaml.load_file(file)
  files <- names(x)
  n_files <- length(x)

  for(i in seq_along(x)) {
    if(i==1) next
    x[[i]] <- merge(x[[i-1]], x[[i]], open = TRUE)
  }

  for(i in seq_along(x)) {
    x[[i]]$name <- files[[i]]
    if(is.null(x[[i]]$spec)) {
      x[[i]]$spec <- paste0(files[[i]], ".yml")
    }
    if(is.null(x[[i]]$source)) {
      x[[i]]$source <- paste0(x[[i]]$name, ".xpt")
    }
    if(is.null(x[[i]]$path)) {
      x[[i]]$path <- '.'
    }
    x[[i]]$file <- file.path(x[[i]]$path, x[[i]]$spec)
    if(!file.exists(x[[i]]$file)) {
      stop("could not find file ", x[[i]]$file)
    }
  }

  x
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

  x <- spec_define(file)
  writeLines(paste0("# ", main))
  writeLines(fda_content_table(x))

  for(i in seq_along(x)) {
    this <- x[[i]]
    title <- paste0(this$description, " (`", this$source, "`)")
    header <- paste0("## ", title, " \\label{", this$name,"}")
    writeLines(header)
    writeLines(fda_table_file(this$file))
  }
}

##' @rdname print_fda_define
##' @export
fda_define <- function(...) {
  capture.output(print_fda_define(...))
}

fda_content_ref <- function(name, source) {
  source <- gsub("_", "\\\\_", source)
  paste0("\\hyperref[",name,"]{", source, "}")
}

as_fda_content_ <- function(x) {
  data_frame(Description  = x$description,
             Location = paste0(fda_content_ref(x$name,x$source)))
}

as_fda_content <- function(x) {
  lapply(x, as_fda_content_) %>% bind_rows
}

##' Print a table of contents for FDA define document
##'
##' @param x a spec define object
##' @param file the full path to a yaml specification file
##' @seealso \code{\link{spec_define}}
##' @export
fda_content_table <- function(x) {
  if(!is.list(x)) stop("x is not a list", call. = FALSE)
  contents <- as_fda_content(x)
  kable(contents,
        format = "latex",
        align = c("|p{2.85in}", "p{2.75in}|"),
        escape = FALSE)
}

##' @rdname fda_content_table
##' @export
fda_content_table_file <- function(file) {
  x <- spec_define(file)
  fda_content_table(x)
}
