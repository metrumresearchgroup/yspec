
##' Extract plotting information
##'
##' @param x data spec object
##' @param name unquoted data spec name
##' @export
xy <- function(x,name) {
  name <- as.character(substitute(name))
  x[[name]][["plot_data"]]
}
