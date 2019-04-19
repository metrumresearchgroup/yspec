
##' Add factors to data set based on spec information
##' 
##' @param .data the data set to modify
##' @param .spec a yspec object
##' @param ... unquoted column names for modification
##' @param .suffix used to make the column name for the factors
##' @param values a vector of values to convert to a factor
##' @param x a ycol object
##' 
##' @details
##' Note that `.suffix` can be chosen using option `ys.fct.suffix`.
##' 
##' @examples
##' 
##' spec <- load_spec_ex()
##' 
##' yspec_make_factor(c(1,0,1,1,1,0), spec$SEX)
##' 
##' data <- data.frame(SEX = c(1,1,1,1,0,0,1,1), STUDY= c(202,100))
##' 
##' yspec_add_factors(data, spec, SEX, STUDY)
##' 
##' @export
yspec_add_factors <- function(.data, .spec, ... , .suffix = 
                                getOption("ys.fct.suffix","_f")) {
  
  assert_that(inherits(.spec, "yspec"))
  
  what <- exprs(...)
  
  if(identical(unname(map_chr(what,as_string)), ".all_vars")) {
    dis <- map_lgl(.spec, ~!is.null(.x[["values"]]))
    vars <- select_vars(names(.data), names(which(dis)))
  } else {
    vars <- select_vars(names(.data), !!!what)
  }
  
  for(v in vars) {
    newcol <- paste0(v, .suffix)
    .data[[newcol]] <- yspec_make_factor(.data[[v]],.spec[[v]])
  }
  .data
}

##' @rdname yspec_add_factors
##' @export
yspec_make_factor <- function(values,x) {
  if(is.null(x[["values"]])) {
    stop("Column: ", x[["col"]], " - values field is not found", call. = FALSE)
  }
  if(!x[["discrete"]]) {
    stop("Column: ", x[["col"]], " is not discrete", call. = FALSE)
  }
  if(is.null(x[["decode"]])) {
    decode <- x[["values"]]
  } else {
    decode <- x[["decode"]]  
  }
  factor(values, levels = x[["values"]], labels = decode)
}

