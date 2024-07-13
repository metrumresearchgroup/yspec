#' @importFrom yaml yaml.load_file as.yaml
#' @importFrom dplyr filter %>% bind_rows
#' @importFrom tidyselect vars_select eval_select eval_rename all_of
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr mutate if_else .data desc rowwise
#' @importFrom rmarkdown render pdf_document html_document
#' @importFrom knitr kable
#' @importFrom xtable xtable
#' @importFrom rlang quos set_names exprs as_string expr quo_get_expr enquo
#' @importFrom rlang is_named warn abort
#' @importFrom assertthat assert_that
#' @importFrom purrr map map_chr map_df map_if map_lgl
#' @importFrom purrr imap imap_chr map_int
#' @importFrom purrr discard compact transpose
#' @importFrom purrr walk walk2 iwalk keep
#' @importFrom purrr flatten flatten_chr modify imodify
#' @importFrom glue glue
#' @importFrom utils type.convert read.csv 
#' @importFrom utils capture.output head tail
#' @importFrom tools toTitleCase
#' @importFrom crayon red green black blue bold italic
#' @importFrom fs path_rel
#' @importFrom stringr fixed str_detect str_split_fixed str_count 
#' 
#' @include utils.R
NULL

globalVariables(c("decode", "unit", "source", "type", "value"))

VALID_SPEC_NAMES <- c(
  "type", "unit", "values", "decode",
  "source", "comment",
  "short", "long", "about", "dots",
  "range", "longvalues", "lookup", 
  "axis", "table", "label", "make_factor", 
  "namespace"
)

VALID_SETUP_NAMES <- c(
  "primary_key", "lookup_file", "extend_file",
  "description", "comment", "sponsor", "projectnumber", 
  "data_path", "data_stem", "name", "spec_file", 
  "spec_path", "glue", "use_internal_db", 
  "import", "character_last","comment_col", 
  "max_nchar_label", "max_nchar_col", "max_nchar_short", 
  "flags"
)

ys_control_defaults <- function() {
  list(
    max_nchar_label = 40,
    max_nchar_short = 40,
    max_nchar_col = 8
  )
}

VALID_NS_NAMES <- c(
  "unit", "short", "label", "long", "decode", "comment"
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
#' - `ys.fct.suffix` the suffix to add to a column name, used by 
#'   [yspec::yspec_add_factors()]
#' - `ys.require.label` if `TRUE`, an error will be generated whenever a column
#'   is specified without a label
#'   
#' **NOTE**: `ys.col.len` was an available in previous versions to set the 
#' maximum number of characters allowable in a column name. This options has 
#' been deprecated.  Please use `max_char_col` in `SETUP__:` instead.
#' 
#' @md
#' @name yspec
"_PACKAGE"
