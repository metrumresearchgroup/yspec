##' @importFrom yaml yaml.load_file
##' @importFrom dplyr filter %>% bind_rows data_frame select_vars
##' @importFrom dplyr mutate if_else
##' @importFrom rmarkdown render pdf_document
##' @importFrom knitr kable
##' @importFrom xtable xtable
##' @importFrom utils capture.output head
##' @importFrom rlang quos set_names
##' @importFrom assertthat assert_that
##' @importFrom purrr map map_chr imap map_df
NULL


globalVariables(c("decode", "unit", "source", "type", "value"))
