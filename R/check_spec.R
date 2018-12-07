
check_values <- function(x,values,verbose=FALSE, con = NULL) {
  if(is.null(values)) return(TRUE)
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
  if(is.null(range)) return(TRUE)
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
  
  if(length(nspec) > length(ndata)) {
    add_log(env, "spec has more items that data")
  }
  if(length(nspec) < length(ndata)) {
    add_log(env, "data has more items that spec")
  }
  
  # In the spec but not in the data
  diff <- setdiff(nspec,ndata)
  pos <- match(diff,nspec)
  dfs <- data.frame(position = pos, col_name = diff,col_source = "spec",stringsAsFactors=FALSE)
  diff <- paste0(diff, collapse = ", ")
  diff <- crayon::black(strwrap(diff, width = 60))
  add_log(env, "names in spec but not in data:")
  append_log(env,diff,.bullet = FALSE)
  
  # In the data but not in the spec
  diff <- setdiff(ndata,nspec)
  pos <- match(diff,ndata)
  dfd <- data.frame(position = pos, col_name = diff,col_source = "data",stringsAsFactors=FALSE)
  diff <- paste0(diff, collapse = ", ")
  diff <- crayon::black(strwrap(diff, width = 60))
  add_log(env, "names in data but not in spec:")
  append_log(env,diff,.bullet = FALSE)
  
  dff <- dplyr::arrange(dplyr::bind_rows(dfs,dfd), .data["position"])
  dff <- capture.output(print(as.data.frame(dff), row.names = FALSE,right=FALSE)) 
  dff <- paste0("  ", dff)
  dff <- paste0(dff,collapse = "\n")
  cata(dff, file = output, sep = "")
  
  return(invisible(FALSE))
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
    cata("  * column: ", x$col, file = output)
    val <- check_values(y,x$values,verbose = FALSE, con=output)
    if(!val) {
      add_log(env, "discrete value out of range:", x$col)
      add_error(env)
    }
    range <- check_range(y,x$range,verbose=FALSE,con = output)
    if(!range) {
      range <- paste0(x$range,collapse = ",")
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

##' @rdname check_data
##' @export
check_data_file <- function(data, file) {
  check_data(data, load_spec(file))
}
