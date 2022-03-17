##' @export
print.ycol <- function(x,...) {
  rnge <- '.'
  if(!is.null(x$range)) {
    rnge <- paste0(x$range[1], " to ", x$range[2])
  }
  x$unit <- ifelse(is.null(x$unit), '.', x$unit)
  name <- c("col", "type", "short", "unit", "range")
  values <- c(x$col, x$type, x$short, x$unit, rnge)
  ans <- tibble(name = name, value = values)
  
  if(x$discrete) {
    ans <- filter(ans, name != "range", name != "unit")
    if(.has("values", x)) {
      valu <- x[["values"]]
      mx <- max(nchar(valu))
      valu <- formatC(valu, width = mx, flag = "-")
      if(.has("decode", x)) {
        valu <- paste(valu, x[["decode"]], sep = " : ")
      }
      if(any(nchar(valu) > 45)) {
        w <- which(nchar(valu) > 45)
        valu[w] <- paste0(substr(valu[w],1,45)," ...")
      }

      valu <- tibble(name = c("value",rep('', length(valu)-1)), 
                         value = as.character(valu))
      ans <- bind_rows(ans,valu)
    }
  }
  ans <- as.data.frame(ans)
  print(ans, row.names = FALSE, right = FALSE)
}

##' @method as.list ycol
##' @export
as.list.ycol <- function(x,...) {
  unclass(x)
}
