handlrs <- list()

handlrs[["value:decode"]] <- function(x) {
  dcods <- names(x)
  lbl <- as.character(rep(dcods, sapply(x, length)))
  ans <- as.list(type.convert(lbl, as.is=TRUE))
  names(ans) <- unlist(x,use.names=FALSE)
  ans
}

handlrs[["decode:value"]] <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- type.convert(as.character(unlist(x,use.names=FALSE)),as.is=TRUE)
  names(ans) <- lbl
  ans
}

handlrs[["look"]] <- function(x) {
  if(!is.list(x)) return(list(lookup=TRUE))
  x[["lookup"]] <- TRUE
  x 
}


