
##' Extract plotting information
##'
##' @param x data spec object
##' @param name unquoted data spec name
##' @export
xy <- function(x,name) {
  name <- as.character(substitute(name))
  x[[name]][["plot_data"]]
}


merge.list <- function(x,y,...,open=FALSE,
                       warn=TRUE,context="object") {

  y <- as.list(y)

  # if(!open) {
  #   y <- y[names(y)!=wild | is.null(names(y))]
  # }

  ## Merge two lists
  common <- intersect(names(x), names(y))
  #common <- common[common != wild]

  x[common] <- y[common]

  if(open)  {
    nw <- !is.element(names(y),names(x)) #| names(y) == wild
    x <- c(x,y[nw])
  } else {
    if(length(common)==0 & warn) {
      warning(paste0("Found nothing to update: ", context), call.=FALSE)
    }
  }
  x
}

is_yspec <- function(x) inherits(x, "yspec")
