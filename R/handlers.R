value_decode <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- as.list(type.convert(lbl, as.is=TRUE))
  names(ans) <- unlist(x,use.names=FALSE)
  ans
}
decode_value <- function(x) {
  dcods <- names(x)
  lbl <- rep(dcods, sapply(x, length))
  ans <- type.convert(unlist(x,use.names=FALSE),as.is=TRUE)
  names(ans) <- lbl
  ans
}
use_lookup <- function(x) {
  message("use_lookup")
  print(x)
  list(lookup = TRUE)  
}

handlrs <- list(
  `decode:value` = decode_value, 
  `value:decode` = value_decode, 
  look = use_lookup
)

