handlrs <- list()

handlrs[["value:decode"]] <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- as.list(type.convert(lbl, as.is=TRUE))
  names(ans) <- unlist(x,use.names=FALSE)
  ans
}

handlrs[["decode:value"]] <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- type.convert(unlist(x,use.names=FALSE),as.is=TRUE)
  names(ans) <- lbl
  ans
}

handlrs[["look"]] <- function(x) {
  list(lookup = TRUE)  
}


