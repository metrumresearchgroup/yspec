

merge.list <- function(x,y,..., open=FALSE,
                       warn=FALSE, context="object") {
  
  y <- as.list(y)
  
  ## Merge two lists
  common <- intersect(names(x), names(y))
  
  x[common] <- y[common]
  
  if(open)  {
    nw <- !is.element(names(y),names(x)) #| names(y) == wild
    x <- c(x,y[nw])
  } else {
    if(length(common)==0 & warn) {
      warning(
        paste0("Found nothing to update: ", context),
        call.=FALSE
      )
    }
  }
  x
}

combine_list <- function(left, right) {
  if(length(right)==0) return(left)
  if(!all(is.list(left),is.list(right))) {
    stop("input are not lists")
  }
  left[names(right)] <-  right
  left
}

update_list <- function(left, right) {
  if(length(right)==0) return(left)
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

make_null <- function(x, name) {
  modify(x, function(xx) {
    xx[[name]] <- NULL
    xx
  })
}

.no <- function(name,object) {
  is.null(object[[name]])
}

.has <- function(name,object) {
  !is.null(object[[name]])
}

.stop <- function(...) stop(..., call. = FALSE)

try_yaml <- function(file) {
  file <- normalizePath(file,mustWork=FALSE)
  if(!file.exists(file)) {
    .stop("the file ", basename(file) ,
          "\n  does not exist in directory\n  ",
          dirname(file))
  }
  this <- try(yaml.load_file(file, handlers=handlrs), silent = TRUE)
  if(is_error(this)) {
    tryfile <- paste0("yaml::yaml.load_file(\"",file,"\")")
    .stop(
      "failed to parse ",
      basename(file),
      "\n\n",
      this,
      "\nplease try running ",
      tryfile,
      " and fix yaml code"
    )
  }
  this
}

scan_yml <- function(file,n) {
  scan(file,comment.char="#",what=character(),quiet=TRUE,n=n)  
}

.test_spec <- function(.name, ..., .where = tempfile()) {
  
  a <- set_names(list(list(type = "numeric", lookup = FALSE)),
                 "AAA")
  b <- set_names(list(list(type = "numeric", short = "just a test",
                           lookup = FALSE)), "BBB")
  x <- set_names(list(list(...)),.name)
  yaml <- as.yaml(c(a, x,b))
  writeLines(yaml, .where)
  return(.where)
}

.test_load <- function(...) {
  suppressMessages(load_spec(.test_spec(...)))
}

yspec_pdf_document <- function(...,template = NULL) {
  
  template <- system.file("tex", "yspectemplate.tex",
                          package = "yspec")
  rmarkdown::pdf_document(..., template = template)
}

yspectemplate <- function() {
  system.file("tex", "yspectemplate.tex",
              package = "yspec")
}

cata <- function(..., fill = TRUE, append = TRUE) {
  cat(..., fill = fill, append = append)  
}

yspec_is_discrete <- function(x) {
  x[["discrete"]]  
}

yspec_is_character <- function(x) {
  x[["type"]] == "character"  
}

yspec_select <- function(.spec, ...) {
  vars <- select_vars(names(.spec), !!!quos(...))
  .spec[vars]
}

yspec_select_if <- function(.spec, .p) {
  if_check <- map_lgl(.spec, .p)
  if(!any(if_check)) {
    stop("No columns matched the select criteria.", call. = FALSE)  
  }
  .spec[if_check]
}

##' Select all columns that are discrete
##' 
##' @param .spec a yspec object
##' 
##' @export
yspec_select_discrete <- function(.spec) {
  yspec_select_if(.spec, yspec_is_discrete)  
}

##' Select all columns that are character
##' 
##' @param .spec a yspec object
##' 
##' @export
yspec_select_chr <- function(.spec) {
  yspec_select_if(.spec, yspec_is_character)  
}

##' Test specification code
##' 
##' For internal / testing use
##' 
##' @param x a list column specification data
##' @export
test_spec_list <- function(x) {
  file <- tempfile()
  y <- yaml::as.yaml(x)
  writeLines(con = file, y)
  yspec::load_spec(file)
}

make_sep <- function(width = 40) {
  line <- paste0(rep("-",width-1),collapse = "")
  paste0("#",line)
}

db_quote <- function(x) {
  paste0("\"", x, "\"")  
}

err_file <- function(file,...) {
  message <- unlist(list(...),use.names=FALSE)
  file <- basename(file)
  file <- paste0("file ", file, " :\n")
  message <- strwrap(message, width = 50)
  message <- paste0(" ", message,collapse = "\n")
  stop(file, message,call.=FALSE)
}
