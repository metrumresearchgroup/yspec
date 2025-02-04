
#' Add factors to data set based on spec information
#' 
#' @param .data the data set to modify.
#' @param .spec a yspec object.
#' @param ... unquoted column names for modification; passing nothing through 
#' `...` will signal for all columns to be considered for factors.
#' @param .all if `TRUE` then any column with a `values` attribute or  where 
#' the `make_factor` field evaluates to `TRUE` will be added as a factor.
#' @param .missing a label to use assign to missing values `NA` when making 
#' the factor; keep this `NULL` (the default) to let missing values be handled
#' naturally by `factor()`.
#' @param .suffix used to make the column name for the factors.
#' 
#' @details
#' Note that `.suffix` can be chosen using option `ys.fct.suffix`. When the 
#' factor is made by [base::factor()], the `exclude` argument is forced to 
#' `character(0)` so that nothing is excluded.
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
#' @seealso [ys_factors()]
#' 
#' @md
#' @export
ys_add_factors <- function(.data, .spec, ... , 
                           .all = TRUE, .missing = NULL,
                           .suffix = getOption("ys.fct.suffix","_f")) {
  
  assert_that(inherits(.spec, "yspec"))
  assert_that(is.null(.missing) || is.character(.missing))
  
  fct_ok <- map_lgl(.spec, ~ isTRUE(.x[["make_factor"]]))
  
  what <- exprs(...)
  
  if(length(what) > 0) {
    what <- names(eval_select(expr(c(...)), .data))
  } 
  
  if(length(what)==0 & isTRUE(.all)) {
    dis <- map_lgl(.spec, ~!is.null(.x[["values"]]))
    spec_cols <- names(which(dis | fct_ok))
    spec_cols <- intersect(spec_cols, names(.data))
    vars <- vars_select(names(.data), all_of(spec_cols))
  } else {
    vars <- vars_select(names(.data), all_of(what)) 
  }
  
  for(v in vars) {
    newcol <- paste0(v, .suffix)
    .data[[newcol]] <- ys_make_factor(
      .data[[v]],
      .spec[[v]],
      strict = !fct_ok[[v]], 
      .missing = .missing
    )
  }
  .data
}

#' @rdname ys_add_factors
#' @export
yspec_add_factors <- ys_add_factors

#' @param values a vector of values to convert to a factor
#' @param x a ycol object
#' @param strict if `FALSE`, then a factor will be returned for any `values` type
#' @rdname ys_add_factors
#' @export
ys_make_factor <- function(values, x, strict = TRUE, .missing = NULL) {
  if(is.factor(values)) {
    return(values)
  }
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
  if(!is.null(.missing) && anyNA(values)) {
    values[is.na(values)] <- .missing
    x[["values"]] <- c(x[["values"]], .missing)
    decode <- c(decode, .missing)
  }
  factor(values, levels = x[["values"]], labels = decode, exclude = character(0))
}

#' @rdname ys_add_factors
#' @export
yspec_make_factor <- ys_make_factor


#' Convert columns to factors
#' 
#' This function works like [ys_add_factors()] with the difference that 
#' the original columns become factors (retaining the original column names)
#' and the original columns are retained with a suffix. You can think of this 
#' as a more convenient form of `ys_add_factors(..., .suffix = "")`. 
#' 
#' @inheritParams ys_add_factors
#' @param .keep_values logical; if `TRUE`, value columns will be retained with
#' a `.suffix`.
#' @param .suffix a suffix to be added to original columns (holding values).
#' 
#' @return
#' The original data frame is returned with columns converted to factors
#' and (possibly) additional columns storing values. 
#' 
#' @details
#' Factor conversion will only take place on source columns that _aren't_
#' already factors. That is, if a column in `data` is already a factor, it 
#' will be ignored. This means the function can be called multiple times on 
#' the same input data, but once a column is converted to factor, it will 
#' cannot be converted again in subsequent calls. 
#' 
#' 
#' @examples
#' 
#' library(dplyr)
#' 
#' spec <- ys_help$spec()
#' data <- ys_help$data()
#' 
#' data <- ys_factors(data, spec)
#' 
#' head(data, 5)
#' 
#' spec$EVID
#' 
#' count(data, EVID, EVID_v)
#' 
#' @seealso [ys_add_factors()]
#' @md
#' @export
ys_factors <- function(data, spec, ...,  
                       .keep_values = TRUE, 
                       .suffix = "_v") {
  
  assert_that(is.data.frame(data))
  assert_that(is_yspec(spec))
  
  if(isTRUE(.keep_values) && identical(.suffix, "")) .suffix <- NULL
  if(is.null(.suffix)) .keep_values <- FALSE
  
  incoming_names <- names(data)
  
  tag <- "__ys@factors__"
  
  # Don't modify anything that is already a factor
  factors <- which(sapply(data, is.factor))
  already_factors <- names(data)[factors]
  
  data <- ys_add_factors(data, spec, ..., .suffix = tag)
  
  # Column indices that contain new factors
  fct_cols <- which(grepl(tag, names(data), fixed = TRUE))
  if(length(fct_cols)==0) return(data)
  # Mangled names of columns that contain new factors
  fct_names <- names(data)[fct_cols]
  
  # Original names of columns to be converted
  col_names <- sub(tag, "", names(data)[fct_cols], fixed = TRUE)
  
  # If an incoming column is factor, we ignore; need to adjust both 
  # col_names and fct_cols
  if(length(already_factors) > 0) {
    drop <- col_names %in% already_factors
    col_names <- col_names[!drop]
    fct_cols <- fct_cols[!drop]
  }
  
  # Indices of original columns
  col_cols <- match(col_names, names(data))
  
  # Nothing left to work on; everything was already a factor
  if(length(col_names)==0) {
    data <- data[, !(grepl(tag, names(data), fixed = TRUE))]
    return(data)
  }
  
  if(isTRUE(.keep_values)) {
    names(data)[col_cols] <- paste0(col_names, .suffix) 
  } 
  
  # Set names back to original
  names(data)[fct_cols] <- col_names
  
  if(!isTRUE(.keep_values)) {
    data[,col_cols] <- NULL 
  }
  
  # Drop any tagged columns
  data <- data[, !(grepl(tag, names(data), fixed = TRUE))]
  
  # Restore column order
  new_names <- names(data)
  select_names <- unique(c(incoming_names, new_names))
  data <- data[, select_names]
  
  return(data)
}
