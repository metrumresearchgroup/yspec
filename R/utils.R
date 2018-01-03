
##' Extract plotting information
##'
##' @param x data spec object
##' @param name unquoted data spec name
##' @export
xy <- function(x,name) {
  name <- as.character(substitute(name))
  x[[name]][["plot_data"]]
}


merge.list <- function(x,y,..., open=FALSE,
                       warn=TRUE, context="object") {

  y <- as.list(y)

  ## Merge two lists
  common <- intersect(names(x), names(y))

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

combine_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

update_list <- function(left, right) {
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  common <- intersect(names(left), names(right))
  left[common] <-  right[common]
  left
}


parens <- function(x) paste0("(",x,")")
brackets <- function(x) paste0("[",x,"]")

backticks <- function(x) paste0("`",x,"`")

is_error <- function(x) inherits(x,"try-error")

.no <- function(name,object) {
  is.null(object[[name]])
}

.has <- function(name,object) {
  !is.null(object[[name]])
}

.stop <- function(...) stop(..., call. = FALSE)

try_yaml <- function(file) {
  this <- try(yaml.load_file(file))
  if(is_error(this)) {
    tryfile <- paste0("yaml::yaml.load_file(\"",file,"\")")
    .stop(
      "failed to parse the file ",
      basename(file),
      "\n",
      "please try running ",
      tryfile,
      " and fix yaml code"
    )
  }
  this
}


