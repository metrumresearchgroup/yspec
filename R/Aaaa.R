#' @importFrom yaml yaml.load_file as.yaml
#' @importFrom dplyr filter %>% bind_rows
#' @importFrom tidyselect vars_select
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate if_else .data desc
#' @importFrom rmarkdown render pdf_document html_document
#' @importFrom knitr kable
#' @importFrom xtable xtable
#' @importFrom rlang quos set_names exprs as_string
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_chr map_df map_if map_lgl
#' @importFrom purrr imap imap_chr map_int
#' @importFrom purrr discard compact transpose
#' @importFrom purrr walk walk2 iwalk 
#' @importFrom purrr flatten flatten_chr modify
#' @importFrom glue glue
#' @importFrom utils type.convert read.csv 
#' @importFrom utils capture.output head tail
#' @importFrom tools toTitleCase
#' @importFrom crayon red green black blue bold italic
#' @importFrom fs path_rel
#' 
#' @include utils.R
NULL

globalVariables(c("decode", "unit", "source", "type", "value"))

VALID_SPEC_NAMES <- c(
  "type", "unit", "values", "decode",
  "source", "comment",
  "short", "long", "about", "dots",
  "range", "longvalues", "lookup", 
  "axis", "table", "label", "make_factor"
)

VALID_SETUP_NAMES <- c(
  "primary_key", "lookup_file", 
  "description", "sponsor", "projectnumber", 
  "data_path", "data_stem", "name", "spec_file", 
  "spec_path", "glue", "use_internal_db", 
  "import", "character_last","comment_col"
)

.glopen <- "<<"
.glclose <- ">>"

#' yspec: data specification documents from yaml
#' 
#' After writing data specification details in yaml format, read in as an 
#' object in R, query data set details, check / validate data sets, and 
#' write out data definition documents.  To get started, check out the 
#' [vignettes](../doc/index.html).
#' 
#' @section Workflow:
#' 
#' - Write your document in yaml format
#'     - Include a `SETUP__:` block at the top to include meta information
#'     - Use `lookup`s from either the internal library or your own lookup file
#'     - Syntax reference [here](../doc/reference.html)
#'     - Use [ys_help] to access examples
#' - Use [ys_load()] to read in the yaml file
#'     - Use the `.verbose` to see more information while this is in progress
#' - Once the `spec` object is loaded
#'     - `spec$col` to see the definition of `col`
#'     - `summary(spec)` to see a summary
#'     - [yspec_add_factors()] to create factors in the data set
#' - Use [ys_check()] to check a data frame against the spec
#' - Use [ys_document()] to render the specification object as a pdf file
#' - Use [ys_project()] to create a collection of individual data specification
#'   objects; this also can be rendered as a pdf file with [ys_document()]
#' - Get help: Use [ys_help] to get further help and documentation in your R 
#'   session
#'     
#' @section Package-wide options:
#' 
#' - `ys.sanitize` a function to use for sanitizing text before processing with 
#'   latex; see [yspec::pander_table()]
#' - `ys.col.len` the maximum number of characters in a data frame column 
#'   name; this defaults to 8 so that columns with 9 or more characters will
#'   generate an error on load. 
#' - `ys.fct.suffix` the suffix to add to a column name, used by 
#'   [yspec::yspec_add_factors()]
#' - `ys.require.label` if `TRUE`, an error will be generated whenever a column
#'   is specified without a label
#' @docType package
#' @md
#' @name yspec
NULL


