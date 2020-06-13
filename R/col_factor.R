
#' Add factors to data set based on spec information
#' 
#' @param .data the data set to modify
#' @param .spec a yspec object
#' @param ... unquoted column names for modification
#' @param .all if `TRUE` then any column with a `values` attribute or  where 
#' the `make_factor` field evaluates to `TRUE` will be added as a factor
#' @param .suffix used to make the column name for the factors
#' @param values a vector of values to convert to a factor
#' @param x a ycol object
#' 
#' @details
#' Note that `.suffix` can be chosen using option `ys.fct.suffix`. 
#' 
#' @examples
#' 
#' spec <- load_spec_ex()
#' 
#' ys_make_factor(c(1,0,1,1,1,0), spec$SEX)
#' 
#' data <- data.frame(SEX = c(1,1,1,1,0,0,1,1), STUDY= c(202,100))
#' 
#' head(ys_add_factors(data, spec, SEX, STUDY))
#' 
#' data <- ys_help$data()
#' spec <- ys_help$spec()
#' 
#' head(ys_add_factors(data, spec))
#' 
#' @export
ys_add_factors <- function(.data, .spec, ... , 
                           .all = TRUE, 
                           .suffix = getOption("ys.fct.suffix","_f")) {
  
  assert_that(inherits(.spec, "yspec"))
  
  fct_ok <- map_lgl(.spec, ~ isTRUE(.x[["make_factor"]]))
  
  what <- exprs(...)
  what <- map_chr(what,as_string)
  if(length(what)==0 & isTRUE(.all)) {
    dis <- map_lgl(.spec, ~!is.null(.x[["values"]]))
    vars <- vars_select(names(.data), names(which(dis | fct_ok)))
  } else {
    vars <- vars_select(names(.data),what) 
  }
  
  for(v in vars) {
    newcol <- paste0(v, .suffix)
    .data[[newcol]] <- ys_make_factor(.data[[v]],.spec[[v]],strict=!fct_ok[[v]])
  }
  .data
}

#' @rdname ys_add_factors
#' @export
yspec_add_factors <- ys_add_factors

#' @param strict if `FALSE`, then an factor will be returned for any `values` type
#' @rdname ys_add_factors
#' @export
ys_make_factor <- function(values,x,strict=TRUE) {
  if(is.null(x[["values"]])) {
    if(!strict) return(factor(values))
    stop("column: ", x[["col"]], " - values field is not found", call. = FALSE)
  }
  if(!x[["discrete"]]) {
    stop("column: ", x[["col"]], " is not discrete", call. = FALSE)
  }
  if(is.null(x[["decode"]])) {
    decode <- x[["values"]]
  } else {
    decode <- x[["decode"]]  
  }
  factor(values, levels = x[["values"]], labels = decode)
}

#' @rdname ys_add_factors
#' @export
yspec_make_factor <- ys_make_factor
