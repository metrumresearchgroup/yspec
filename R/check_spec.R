
check_values <- function(x,values) {
  if(is.null(values)) return(TRUE)
  x <- unlist(unique(x),use.names = FALSE)
  if(length(x) != length(values)) return(FALSE)
  length(setdiff(x,values))==0
}

check_range <- function(x,range) {
  if(is.null(range)) return(TRUE)
  if(length(range) !=2) return(FALSE)
  x <- sort(range(x, na.rm = TRUE))
  range <- sort(range)
  x[1] > range[1] & x[2] < range[2]
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
##' @param file the full path to a yaml specification file
##' @export
check_data <- function(data, spec) {
  env <- new.env()
  env$log <- character(0)
  env$error <- FALSE
  nspec <- names(spec)
  ndata <- names(data)

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
    val <- check_values(y,x$values)
    if(!val) {
      add_log(env, "Discrete column out of range: ", x$col)
      add_error(env)
    }
    range <- check_range(y,x$range)
    if(!range) {
      add_log(env, "Continuous column out of range: ", x$col)
      add_error(env)
    }
  }

  if(env$error) {
    message("")
    message("Messages:")
    message(paste(" ",env$log, collapse = "\n"))
    message("")
    stop("please review messages and re-check", call. = FALSE)
  }
  message("Everything checks out!")
  return(invisible(env$error))
}

##' @rdname check_data
##' @export
check_data_file <- function(data, file) {
  check_data(data, load_spec(file))
}
