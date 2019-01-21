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
addin_view_db <- function() {
  file <- system.file("internal", "ysdb_internal.yml", package = "yspec")
  rstudioapi::navigateToFile(
    temp_copy(file, pattern = "ysdb", fileext=".yml")
  )
}
addin_view_example <- function() {
  file <- system.file("spec", "analysis1.yml", package = "yspec")
  rstudioapi::navigateToFile(
    temp_copy(file, pattern = "analysis1", fileext=".yml")
  )
}
# nocov end
