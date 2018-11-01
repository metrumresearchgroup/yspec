
check_values <- function(x,values,verbose=FALSE, con = NULL) {
  if(is.null(values)) return(TRUE)
  x <- x[!is.na(x)]
  x <- unlist(unique(x),use.names = FALSE)
  
  if(verbose | !is.null(con)) {
    valu <- values
    if(length(values) > 3) {
      valu <- c(valu[seq(3)], "...")
    }
    if(verbose) message("  values: ", paste0(valu, collapse = ','))
    if(!is.null(con)) {
      cata( "  values: ", paste0(valu, collapse = ','),file = con)
    }
  }
  if(length(x)==0) return(TRUE)
  all(x %in% values)
}

check_range <- function(x,range,verbose=FALSE, con = NULL) {
  if(is.null(range)) return(TRUE)
  if(length(range) !=2) return(FALSE)
  x <- x[!is.na(x)]
  if(length(x)==0) return(TRUE)
  if(verbose | !is.null(con)) {
    if(verbose) message("  range: ", paste0(range, collapse = ","))
    if(!is.null(con)) {
      cata("  range: ", paste0(range, collapse = ","),file = con)
    }
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
##' @param output the name of a file or a connection 
##' for writing check results
##' @param file the full path to a yaml specification file
##' @export
check_data <- function(data, spec, verbose = FALSE, 
                       output = tempfile()) {
  if(verbose) {
    output <- stderr() 
  }
  env <- new.env()
  env$log <- character(0)
  env$error <- FALSE
  nspec <- names(spec)
  ndata <- names(data)
  data <- as.data.frame(data)
  
  using_stderr <- identical(output,stderr())
  
  if(!using_stderr) {
    cat("#", date(), "\n", file = output)
    cata("# scroll to the bottom of this file for any check error messages\n",
        file = output)
  }
  
  cata("checking that names in the spec and the data set match\n",
       file = output)
  
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
    cata("column: ", x$col, file = output)
    val <- check_values(y,x$values,verbose = verbose, con=output)
    if(!val) {
      add_log(env, "discrete value out of range:", x$col)
      add_error(env)
    }
    range <- check_range(y,x$range,verbose=verbose,con = output)
    if(!range) {
      range <- paste0(x$range,collapse = ",")
      range <- paste0("[",range,"]")
      add_log(env, "continuous value out of range:", x$col, range)
      add_error(env)
    }
  }
  
  if(env$error) {
    cata("", file = output)
    cata("Messages:", file = output)
    cata(paste(" ",env$log, collapse = "\n"), file = output)
    cata("", file = output)
    if(!using_stderr) {
      message("")
      message("Messages:")
      message(paste(" ",env$log, collapse = "\n"))
      message("", file = stderr())
      message("Error: please review messages and re-check")
    }
    .stop("please review messages and re-check")
  }
  
  cata("\nThe data set passed all checks.", file = output) 
  
  if(!using_stderr) {
    message("The data set passed all checks.")
  }
  
  return(invisible(env$error))
}

##' @rdname check_data
##' @export
check_data_file <- function(data, file) {
  check_data(data, load_spec(file))
}
