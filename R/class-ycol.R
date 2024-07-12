##' @export
print.ycol <- function(x,...) {
  rnge <- '.'
  if(!is.null(x$range)) {
    rnge <- paste0(x$range[1], " to ", x$range[2])
  }
  x$unit <- ifelse(is.null(x$unit), '.', x$unit)
  name <- c("col", "type", "short", "unit", "range")
  values <- c(x$col, x$type, x$short, x$unit, rnge)
  ans <- tibble(name = name, value = values)
  
  if(x$discrete) {
    ans <- filter(ans, name != "range", name != "unit")
    if(.has("values", x)) {
      valu <- x[["values"]]
      mx <- max(nchar(valu))
      valu <- formatC(valu, width = mx, flag = "-")
      if(.has("decode", x)) {
        valu <- paste(valu, x[["decode"]], sep = " : ")
      }
      if(any(nchar(valu) > 45)) {
        w <- which(nchar(valu) > 45)
        valu[w] <- paste0(substr(valu[w],1,45)," ...")
      }

      valu <- tibble(name = c("value",rep('', length(valu)-1)), 
                         value = as.character(valu))
      ans <- bind_rows(ans,valu)
    }
  }
  ans <- as.data.frame(ans)
  print(ans, row.names = FALSE, right = FALSE)
}

##' @method as.list ycol
##' @export
as.list.ycol <- function(x,...) {
  unclass(x)
}


#' Extract vector of values from discrete data columns
#' 
#' @param x a yspec object. 
#' @param col the name of a column to extract. 
#' @param drop values to drop; can be integer positions or decode labels.
#' @param named if `TRUE`, return will be named with decode labels if they
#' exist. 
#' 
#' @export
get_values <- function(x, col, drop = NULL, named = TRUE) {
  if(length(col) != 1) {
    abort("`col` must be length 1.")  
  }
  if(!is.character(col)) {
    abort("`col` must have character type.")  
  }
  if(!col %in% names(x)) {
    abort(glue::glue("column {col} not found in `x`."))  
  }
  x <- x[[col]]
  if(!isTRUE(x$discrete)) {
    abort(glue::glue("column `{col}` is not discrete."))  
  }
  ans <- x$values
  if(is.null(x$decode)) {
    decode <- as.character(seq(length(ans)))
    named <- FALSE
  } else {
    decode <- x$decode  
  }
  if(is.numeric(drop)) {
    if(!all(drop %in% seq_along(ans))) {
      abort("numeric `drop` is out of bounds.")  
    }
    ans <- ans[-drop]
    decode <- decode[-drop]
  }
  if(!length(ans)) {
    abort("all values were dropped; nothing to return.")  
  }
  if(!isTRUE(named)) {
    return(ans)  
  }
  names(ans) <- decode
  if(is.character(drop)) {
    if(!all(drop %in% names(ans))) {
      abort("`drop` name is not in the decode vector.")  
    }
    ans <- ans[setdiff(names(ans), drop)]
  }
  if(!isTRUE(named)) {
    ans <- unname(ans)  
  }
  ans
}