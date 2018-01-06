##' @importFrom yaml yaml.load_file as.yaml
##' @importFrom dplyr filter %>% bind_rows data_frame select_vars
##' @importFrom dplyr mutate if_else
##' @importFrom rmarkdown render pdf_document html_document
##' @importFrom knitr kable
##' @importFrom xtable xtable
##' @importFrom utils capture.output head tail
##' @importFrom rlang quos set_names
##' @importFrom assertthat assert_that
##' @importFrom purrr map map_chr map_df map_if
##' @importFrom purrr imap imap_chr
##' @importFrom purrr discard
##' @importFrom purrr walk walk2 iwalk
##' @importFrom glue glue
NULL


globalVariables(c("decode", "unit", "source", "type", "value"))
