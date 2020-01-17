
check_values <- function(x,values,verbose=FALSE, con = NULL) {
  if(is.null(values)  | is.null(x)) return(TRUE)
  x <- x[!is.na(x)]
  x <- unlist(unique(x),use.names = FALSE)
  
  if(verbose | !is.null(con)) {
    valu <- values
    if(length(values) > 3) {
      valu <- c(valu[seq(3)], "...")
    }
    if(verbose) message("    values: ", paste0(valu, collapse = ','))
    if(!is.null(con)) {
      cata( "    values: ", paste0(valu, collapse = ','),file = con)
    }
  }
  if(length(x)==0) return(TRUE)
  all(x %in% values)
}

check_range <- function(x,range,verbose=FALSE, con = NULL) {
  if(is.null(range) | is.null(x)) return(TRUE)
  if(length(range) !=2) return(FALSE)
  x <- x[!is.na(x)]
  if(length(x)==0) return(TRUE)
  if(verbose | !is.null(con)) {
    if(verbose) message("    range: ", paste0(range, collapse = ","))
    if(!is.null(con)) {
      cata("    range: ", paste0(range, collapse = ","),file = con)
    }
  }
  x <- sort(range(x))
  range <- sort(range)
  x[1] >= range[1] & x[2] <= range[2]
}

add_log <- function(env,...,.bullet = TRUE) {
  msg <- as.character(list(...))
  msg <- paste(msg, collapse = " ")
  if(.bullet) {
    prefix <- "- "
  } else {
    prefix <- "  "  
  }
  env$log <- c(env$log, paste0(prefix, msg))
  return(invisible(NULL))
}

append_log <- function(env,entries,.bullet = FALSE) {
  if(.bullet) {
    prefix <- "- "
  } else {
    prefix <- "  "  
  }
  env$log <- c(env$log, paste0(prefix, entries))
  return(invisible(NULL))
  
}



add_error <- function(env) {
  env$error <- TRUE
  return(invisible(NULL))
}

check_data_names <- function(ndata,nspec,env,output) {
  if(identical(nspec,ndata)) {
    cata(" * names for data and spec are identical.",sep="",file=output)
    return(invisible(TRUE))
  }
  
  cata(" * names in spec do not match names in data.",sep="",file=output)
  
  add_error(env)
  
  if(identical(sort(nspec),sort(ndata))) {
    add_log(env, "data cols are not sorted according to the spec")
    return(invisible(FALSE))
  }
  
  if(length(nspec) > length(ndata)) {
    add_log(env, "spec has more items that cols in the data")
  }
  if(length(nspec) < length(ndata)) {
    add_log(env, "data has more cols than items in the spec")
  }
  
  # In the spec but not in the data
  diff <- setdiff(nspec,ndata)
  dfs <- data.frame()
  if(length(diff) > 0) {
    pos <- match(diff,nspec)
    dfs <- data.frame(position = pos, col_name = diff,col_source = "spec",stringsAsFactors=FALSE)
    diff <- paste0(diff, collapse = ", ")
    diff <- crayon::black(strwrap(diff, width = 50))
    add_log(env, "names in spec but not in data:")
    append_log(env,diff,.bullet = FALSE)
  }
  
  # In the data but not in the spec
  diff <- setdiff(ndata,nspec)
  dfd <- data.frame()
  if(length(diff) > 0) {
    pos <- match(diff,ndata)
    dfd <- data.frame(position = pos, col_name = diff,col_source = "data",stringsAsFactors=FALSE)
    diff <- paste0(diff, collapse = ", ")
    diff <- crayon::black(strwrap(diff, width = 50))
    add_log(env, "names in data but not in spec:")
    append_log(env,diff,.bullet = FALSE)
  }
  
  dff <- dplyr::bind_rows(dfs,dfd)
  dff <- dplyr::arrange(dff,.data[["position"]], desc(.data[["col_source"]]))
  dff <- capture.output(print(as.data.frame(dff), row.names = FALSE,right=FALSE)) 
  dff <- paste0("  ", dff)
  dff <- paste0(dff,collapse = "\n")
  cata(dff, file = output, sep = "")
  
  return(invisible(FALSE))
}



#' Check a data set against its specification
#' 
#' See the check details below.
#'
#' @param data a data frame
#' @param spec a yspec object
#' @param verbose `logical`; if `TRUE`, extra messages
#' are printed during the check
#' @param output the name of a file or a connection 
#' for writing check results
#' @param file the full path to a yaml specification file
#' @param ... arguments passed from alias function to preferred function name
#' 
#' @details
#' 
#' To pass the data check, all of the following must be true:
#' 
#' 1. The (column) names in the data set must be identical to the 
#'    names in the spec object.
#' 1. For discrete data types (where `values` is set), the unique values
#'    in the data set column after removing missing values must be identical
#'    to or a subset of the values given in the spec object.
#' 1. For continuous data types where a `range` is given, all of the 
#'    values in the data set column must be greater than the lower bound
#'    of the range and less than the upper bound of the range, inclusive, 
#'    after removing missing values.
#'    
#' Other checks are implicit in the data specification object and are checked
#' on load:
#' 
#' 1. All column names must be less than or equal to 8 characters by default.
#'    This maximum number of characters can be overridden by setting
#'    option `ys.col.len` equal to the desired maximum.
#'    
#' Output can be directed to a file (see the `output` argument) and 
#' more verbose output can be requested as the check proceeds by the 
#' `verbose` argument.
#' 
#' @examples
#' 
#' data <- ys_help$data()
#' spec <- ys_help$spec()
#' 
#' # Recommend running this at the end of data assembly and will fix an error
#' # stating that the data cols are not sorted according to the spec
#' data <- dplyr::select(data,names(spec))
#' 
#' ys_check(data,spec)
#' 
#' @md
#' @export
ys_check <- function(data, spec, verbose = FALSE, output = tempfile()) {
  
  if(!is.data.frame(data)) {
    stop("'data' argument must be a data frame.", call.=FALSE)  
  }
  
  if(!is_yspec(spec)) {
    stop("'spec' argument must be a yspec object.", call.=FALSE)  
  }
  
  env <- new.env()
  env$log <- character(0)
  env$error <- FALSE
  nspec <- names(spec)
  ndata <- names(data)
  data <- as.data.frame(data)
  
  using_stderr <- identical(output,stderr())
  
  cat("#", date(), "\n", file = output)
  cata("# scroll to the bottom of this file for any check error messages",
       file = output)
  
  cata(make_sep(), "\n", file = output,sep="")
  
  cata("checking that names in the spec and the data set match:",
       file = output, sep="")
  
  check_data_names(ndata,nspec,env,output) 

  cata("\n", make_sep(), "\n", file = output,sep="")
  
  cata("checking each column in the spec:", file = output)
  
  for(i in seq_along(spec)) {
    x <- spec[[i]]
    y <- data[[x$col]]
    if(is.null(y)) next 
    cata("  * column: ", x$col, file = output)
    val <- check_values(y,x[["values"]],verbose = FALSE, con=output)
    if(!val) {
      add_log(env, "discrete value out of range:", x$col)
      add_error(env)
    }
    range <- check_range(y,x[["range"]],verbose=FALSE,con = output)
    if(!range) {
      range <- paste0(x[["range"]],collapse = ",")
      range <- paste0("[",range,"]")
      add_log(env, "continuous value out of range:", x$col, range)
      add_error(env)
    }
  }
  
  cata("\n", make_sep(), "\n", file = output,sep="")
  
  
  if(env$error) {
    msgs <- c(
      crayon::bold(crayon::green("Messages:\n")),
      paste0(paste0(" ", env$log), collapse = "\n"), 
      ""
    )
    cata(crayon::strip_style(msgs), file = output,sep="")
    if(verbose) {
      message(paste0(readLines(output),"\n"))  
    } else {
      cat("\n")
      message(msgs)
      cat("\n")
    }
    message(make_sep())
    .stop("Please review messages and re-check.")
  }
  
  if(verbose) {
    message(paste0(readLines(output),"\n"))  
  }
  
  cata("\nThe data set passed all checks.", file = output) 
  
  if(!using_stderr) {
    message("The data set passed all checks.")
  }
  return(invisible(env$error))
}

#' @rdname ys_check
#' @export
ys_check_file <- function(data, file) {
  ys_check(data, load_spec(file))
}

#' @rdname ys_check
#' @export
check_data <- function(...) ys_check(...)

#' @rdname ys_check
#' @export
check_data_file <- function(...) ys_check_file(...)


