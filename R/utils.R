
#' @noRd
#' @export
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
ys_update_list <- update_list

# nocov start
parens <- function(x) paste0("(",x,")")

brackets <- function(x) paste0("[",x,"]")

backticks <- function(x) paste0("`",x,"`")

# nocov end

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

.has <- function(name, object) {
  !is.null(object[[name]])
}

.stop <- function(...) stop(..., call. = FALSE)

try_yaml <- function(file) {
  file <- normalPath(file,mustWork=FALSE)
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

# nocov start
yspec_pdf_document <- function(...,template = NULL) {
  
  template <- system.file("tex", "yspectemplate.tex",
                          package = "yspec")
  rmarkdown::pdf_document(..., template = template)
}

yspectemplate <- function() {
  system.file("tex", "yspectemplate.tex",
              package = "yspec")
}
# nocov end

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
  vars <- vars_select(names(.spec), !!!quos(...))
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
##' @keywords internal
##' @export
test_spec_list <- function(x,setup=list()) {
  file <- tempfile()
  set <- list(SETUP__=setup)
  y <- yaml::as.yaml(c(set,x))
  writeLines(con = file, y)
  yspec::ys_load(file)
}

test_spec_error <- function(x) {
  error <- system.file(
    "spec", "error", 
    x, 
    package = "yspec"
  )
  ys_load(error)
}

test_spec_test <- function(x) {
  test <- system.file(
    "spec", "test", 
    x, 
    package = "yspec"
  )
  ys_load(test)
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

warn_file <- function(file,...) {
  message <- unlist(list(...),use.names=FALSE)
  file <- basename(file)
  file <- paste0("file ", file, " :\n")
  message <- strwrap(message, width = 50)
  message <- paste0(" ", message,collapse = "\n")
  warning(file, message,call.=FALSE, immediate.=TRUE)
}

temp_copy <- function(file,pattern,fileext=".yml") {
  tmp <- tempfile(pattern=pattern,fileext=fileext)
  x <- file.copy(file,tmp)
  return(tmp)
}

normalPath <- function(path, winslash = .Platform$file.sep, mustWork = NA) {
  normalizePath(path=path, winslash=winslash, mustWork=mustWork)  
}

##' Functions to sanitize text for TeX documents
##' 
##' Based on [xtable::sanitize()].  
##' 
##' @param x text to sanitize
##' 
##' @examples
##' ys_sanitize("Concentration ($\\mu$g)")
##' ys_sanitize("H&M")
##' 
##' @md
##' @export
ys_sanitize <- function(x) {
  result <- x
  result <- gsub("%", "\\%", result, fixed = TRUE)
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("_", "\\_", result, fixed = TRUE)
  result <- gsub("#", "\\#", result, fixed = TRUE)
  result <- gsub("|", "$|$", result, fixed = TRUE)
  result <- gsub("{", "\\{", result, fixed = TRUE)
  result <- gsub("}", "\\}", result, fixed = TRUE)
  result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
  result
}

##' @rdname ys_sanitize
##' @export
ys_dont_sanitize <- function(x) {
  x
}

##' @rdname ys_sanitize
##' @export
ys_mild_sanitize <- function(x) {
  result <- x
  result <- gsub("%", "\\%", result, fixed = TRUE)
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("#", "\\#", result, fixed = TRUE)
  result <- gsub("|", "$|$", result, fixed = TRUE)
  result <- gsub("{", "\\{", result, fixed = TRUE)
  result <- gsub("}", "\\}", result, fixed = TRUE)
  result  
}


verb <- function(left, right) {
  left <- paste0(crayon::green(formatC(left, width = 14,flag="-")), "... ")
  cat(left,right,"\n")
}

relapse <- function(x,n) paste0(rep(x,n),collapse="")

cvec_cs <- function (x) {
  if (is.null(x) | length(x) == 0) 
    return(character(0))
  x <- unlist(strsplit(as.character(x), ",", fixed = TRUE), 
              use.names = FALSE)
  x <- unlist(strsplit(x, " ", fixed = TRUE), use.names = FALSE)
  x <- x[x != ""]
  if (length(x) == 0) {
    return(character(0))
  }
  else {
    return(x)
  }
}

do_escape <- function(x) {
  str_count(x, fixed("$")) <= 1 &
    str_count(x, fixed("\\")) == 0
}

ys_escape <- function(string, esc = c("_", "%", "$", "&"), ...) {
  if(is.null(esc)) return(string)
  w <- do_escape(string)
  for(ch in esc) {
    string[w] <- gsub(ch, paste0("\\",ch), string[w], fixed = TRUE)
  }
  string
}

expand_names_on_colon <- function(set, valid) {
  set <- str_split_fixed(set, ":", n = 2)
  set <- as.data.frame(set, stringsAsFactors = FALSE)
  set <- mutate(
    set, 
    V2 = ifelse(.data[["V2"]] == "", .data[["V1"]], .data[["V2"]])
  )
  set <- mutate(set, V3 = match(.data[["V1"]], valid))
  set <- mutate(set, V4 = match(.data[["V2"]], valid))
  bad1 <- dplyr::filter(set, is.na(.data[["V3"]]))
  bad1 <- bad1[["V1"]]
  bad2 <- dplyr::filter(set, is.na(.data[["V4"]]))
  bad2 <- bad2[["V2"]]
  bad <- unique(c(bad1, bad2))
  set <- filter(set, !is.na(.data[["V3"]]) & !is.na(.data[["V4"]]))
  if(nrow(set) > 0) {
    set <- mutate(
      rowwise(set), 
      selected = list(valid[sort(seq(.data[["V3"]], .data[["V4"]]))])
    )
    cols <- unique(unlist(set[["selected"]]))
  } else {
    cols <- character(0)  
  }
  list(cols = cols, bad_cols = bad, any_bad = length(bad) > 0)
}

temp_spec <- function(text, name) {
  file <- file.path(tempdir(), name)
  writeLines(text, con = file)
  return(invisible(file))
}

yspec_glue <- function(spec, txt) {
  glu <- get_meta(spec)[["glue"]]
  if (is.list(glu)) {
    txt <- sapply(
      txt, glue::glue_data,
      .x = glu, .envir = emptyenv(), .open = .glopen, .close = .glclose
    )
  }

  return(txt)
}
