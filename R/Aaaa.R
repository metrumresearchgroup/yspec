##' @importFrom yaml yaml.load_file as.yaml
##' @importFrom dplyr filter %>% bind_rows data_frame select_vars
##' @importFrom dplyr mutate if_else .data desc
##' @importFrom dplyr tibble as_tibble
##' @importFrom rmarkdown render pdf_document html_document
##' @importFrom knitr kable
##' @importFrom xtable xtable
##' @importFrom utils capture.output head tail
##' @importFrom rlang quos set_names
##' @importFrom assertthat assert_that
##' @importFrom purrr map map_chr map_df map_if map_lgl
##' @importFrom purrr imap imap_chr map_int
##' @importFrom purrr discard compact transpose
##' @importFrom purrr walk walk2 iwalk 
##' @importFrom purrr flatten flatten_chr modify
##' @importFrom glue glue
##' @importFrom utils type.convert read.csv
##' @importFrom crayon red green black
##' 
##' @include utils.R
NULL

globalVariables(c("decode", "unit", "source", "type", "value"))

VALID_SPEC_NAMES <- c(
  "type", "unit", "values", "decode",
  "source", "comment",
  "short", "long", "about", "dots",
  "range", "longvalues", "lookup", 
  "axis", "table"
)

VALID_SETUP_NAMES <- c(
  "primary_key", "lookup_file", 
  "description", "sponsor", "projectnumber", 
  "data_path", "data_stem", "name", "spec_file", 
  "spec_path", "glue", "use_internal_db"
)

.glopen <- "<<"
.glclose <- ">>"

#' yspec: data specification documents from yaml.
#' 
#' 
#' @section Options:
#' 
#' - `ys.sanitize` a function to use for sanitizing text before processing wiht 
#'   latex; see [yspec::pander_table()]
#' - `ys.col.length` the maximum number of characters in a data frame column 
#'   name; this defaults to 8 so that columns with 9 or more characters will
#'   generate an error on load. 
#' - `ys.fct.suffix` the suffix to add to a column name, used by 
#'   [yspec::yspec_add_factors()]
#' 
#' 
#' 
#' @docType package
#' @md
#' @name yspec
NULL