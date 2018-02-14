
check_values <- function(x,values,verbose=FALSE) {
  if(is.null(values)) return(TRUE)
  x <- x[!is.na(x)]
  x <- unlist(unique(x),use.names = FALSE)
  if(verbose) {
    valu <- values
    if(length(values) > 3) {
      valu <- c(valu[seq(3)], "...")
    }
    message("  values: ", paste0(valu, collapse = ','))
  }
  if(length(x) != length(values)) return(FALSE)
  length(setdiff(x,values))==0
}

check_range <- function(x,range,verbose=FALSE) {
  if(is.null(range)) return(TRUE)
  if(length(range) !=2) return(FALSE)
  x <- x[!is.na(x)]
  if(length(x)==0) return(TRUE)
  if(verbose) {
    message("  range: ", paste0(range, collapse = ","))
  }
  x <- sort(range(x))
  range <- sort(range)
  x[1] >= range[1] & x[2] <= range[2]
}

add_log <- function(env,...) {
  msg <- as.character(list(...))
  msg <- paste(msg, collapse = " ")
  env$log <- c(env$log, paste0("- ", msg))
  return(invisible(NULL))
}

add_error <- function(env) {
  env$error <- TRUE
  return(invisible(NULL))
}


##' Check a data set against its specification
##'
##' @param data a data frame
##' @param spec a yspec object
##' @param verbose \code{logical}; if \code{TRUE}, extra messages
##' are printed during the check
##' @param file the full path to a yaml specification file
##' @export
check_data <- function(data, spec, verbose = FALSE) {
  env <- new.env()
  env$log <- character(0)
  env$error <- FALSE
  nspec <- names(spec)
  ndata <- names(data)
  data <- as.data.frame(data)
  
  if(verbose) {
    message("checking that names in the spec and the data set match")
  }
  
  if(!identical(nspec, ndata)) {
    add_log(env, "data names do not match names in spec")
    add_error(env)
    
    if(length(nspec) > length(ndata)) {
      add_log(env, "spec has more items that data")
    }
    if(length(nspec) < length(ndata)) {
      add_log(env, "data has more items that spec")
    }
  }
  
  for(i in seq_along(spec)) {
    x <- spec[[i]]
    y <- data[,x$col]
    if(verbose) message("checking column: ", x$col)
    val <- check_values(y,x$values,verbose=verbose)
    if(!val) {
      add_log(env, "discrete value out of range:", x$col)
      add_error(env)
    }
    range <- check_range(y,x$range,verbose=verbose)
    if(!range) {
      range <- paste0(x$range,collapse = ",")
      range <- paste0("[",range,"]")
      add_log(env, "continuous value out of range:", x$col, range)
      add_error(env)
    }
  }
  
  if(env$error) {
    message("")
    message("Messages:")
    message(paste(" ",env$log, collapse = "\n"))
    message("")
    .stop("please review messages and re-check")
  }
  message("Everything checks out!")
  
  return(invisible(env$error))
}

##' @rdname check_data
##' @export
check_data_file <- function(data, file) {
  check_data(data, load_spec(file))
}
