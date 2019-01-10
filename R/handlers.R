value_name <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- as.list(type.convert(lbl, as.is=TRUE))
  names(ans) <- unlist(x,use.names=FALSE)
  ans
}
name_value <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- type.convert(unlist(x,use.names=FALSE),as.is=TRUE)
  names(ans) <- lbl
  ans
}


handlrs <- list(`name:value` = name_value, `value:name` = value_name)

