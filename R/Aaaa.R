##' @importFrom yaml yaml.load_file as.yaml
##' @importFrom dplyr filter %>% bind_rows data_frame select_vars
##' @importFrom dplyr mutate if_else .data desc
##' @importFrom rmarkdown render pdf_document html_document
##' @importFrom knitr kable
##' @importFrom xtable xtable
##' @importFrom utils capture.output head tail
##' @importFrom rlang quos set_names
##' @importFrom assertthat assert_that
##' @importFrom purrr map map_chr map_df map_if map_lgl
##' @importFrom purrr imap imap_chr
##' @importFrom purrr discard compact transpose
##' @importFrom purrr walk walk2 iwalk
##' @importFrom purrr flatten flatten_chr modify
##' @importFrom glue glue
NULL


globalVariables(c("decode", "unit", "source", "type", "value"))

yspec_internal <- new.env()
yspec_internal$last_check <- ""

