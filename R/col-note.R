#' Create table note text for abbreviated columns
#' 
#' Produces a string with `<col-name>: <col-definition>` format which can be 
#' included in a table note providing a more informative definition with the 
#' column name. The column definition can be generated from the `short` name
#' or the `label` (but note that these could frequently be the same).
#' 
#' @param .spec a yspec object. 
#' @param ... passed to [ys_select()].
#' @param .unit logical; if `TRUE`, then append the unit surrounded by parens
#' to the column definition.
#' @param .title_case logical; if `TRUE` then the column definition text is 
#' passed through [tools::toTitleCaase()].
#' @param .sep a separator character for columns. 
#' @param .to_string logical; if `TRUE`, then a single string is returned. 
#' @param .width if `numeric` and `.to_string` is `TRUE`, then the result is 
#' passed through [base::strwrap()].
#' @param .type selects if the column definition is generated from calling 
#' [ys_get_short()] or [ys_get_label()]
#' 
#' @return 
#' A string of length one when `.to_string` is `TRUE` or a character vector
#' if `.to_string` is `FALSE`.
#' 
#' @examples
#' 
#' spec <- ys_help$spec()
#' 
#' ys_col_note(spec, AST, ALT, SCR, .unit = TRUE) 
#' 
#' @md
#' @export
ys_col_note <- function(.spec, ..., .unit = FALSE, .title_case = FALSE, 
                        .sep = "; ", .to_string = TRUE, .width = NULL, 
                        .type = c("short", "label")) {
  .spec <- ys_select(.spec, ...) 
  if(length(.spec)==0) return(NULL)
  .type <- match.arg(.type)
  if(.type=="short") {
    sh <- unlist(ys_get_short(.spec))   
  }
  if(.type=="label") {
    sh <- unlist(ys_get_label(.spec))  
  }
  if(isTRUE(.title_case)) {
    sh <- toTitleCase(sh)  
  }
  if(isTRUE(.unit)) {
    u <- ys_get_unit(.spec, parens = TRUE)
    sh <- paste0(sh, " ", u)
  }
  ans <- paste0(names(.spec), ": ", unname(sh))
  if(isTRUE(.to_string)) {
    ans <- paste0(ans, collapse = .sep)
    if(is.numeric(.width)) {
      ans <- strwrap(ans, width = .width) 
    }
  }
  ans
}
