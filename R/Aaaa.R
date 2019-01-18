##' @importFrom yaml yaml.load_file as.yaml
##' @importFrom dplyr filter %>% bind_rows data_frame select_vars
##' @importFrom dplyr mutate if_else .data desc
##' @importFrom dplyr tibble
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
##' @importFrom utils type.convert read.csv
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

.ys <- new.env()
.onLoad <- function(libname, pkgname) {
  file <- system.file("spec", "analysis1.yml", package = "yspec")
  .ys[["file"]] <- file
  .ys[["spec"]] <- function() {
    ys_load(file)
  }
  .ys[["proj"]] <- function() {
    load_proj_ex("project.yml")
  }
  .ys[["data"]] <- function() {
    read.csv(
      file=system.file("internal", "analysis1.csv", package = "yspec"),
      na.strings = '.', as.is=TRUE, stringsAsFactors=FALSE
    )    
  }
}

