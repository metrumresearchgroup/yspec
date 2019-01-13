##' @importFrom rstudioapi insertText
# nocov start
insertNUMERICAddin <- function() {
  insertText("NAME:\n")
  insertText("  short:\n")
  insertText("  unit: \n")
  insertText("  range: [0,Inf]\n")
  insertText("  type: numeric\n")
}
insertCHARACTERAddin <- function() {
  insertText("NAME:\n")
  insertText("  short:\n")
  insertText("  values:\n")
  insertText("    - \n")
  insertText("  decode:\n")
  insertText("    -\n")
  insertText("  type: character\n")
}
# nocov end
