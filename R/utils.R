

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
      warning(
        paste0("Found nothing to update: ", context),
        call.=FALSE
      )
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
  if(!file.exists(file)) {
    .stop("the file ", basename(file) ,
          "\n  does not exist in directory\n  ",
          dirname(file))
  }
  this <- try(yaml.load_file(file), silent = TRUE)
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


mrgtemplate <- function(root = tempdir()) {
  dir <- file.path(root, "mrgtemplate")
  if(!dir.exists(dir)) dir.create(dir)
  templ_dir <- system.file(
    "mrgtemplate", package = "yspec"
  )
  files <- list.files(templ_dir, full.names=TRUE)
  foo <- file.copy(files, dir, overwrite = TRUE)
  dir
}
