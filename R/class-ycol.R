##' @export
print.ycol <- function(x,...) {
  rnge <- '.'
  if(!is.null(x$range)) {
    rnge <- paste0(x$range[1], " to ", x$range[2])
  }
  x$unit <- ifelse(is.null(x$unit), '.', x$unit)
  name <- c("col", "type", "short", "unit", "range")
  values <- c(x$col, x$type, x$short, x$unit, rnge)
  ans <- data.frame(name = name, value = values)
  print(ans, row.names = FALSE, right = FALSE)
}

##' @method as.list ycol
##' @export
as.list.ycol <- function(x,...) {
  unclass(x)
}
