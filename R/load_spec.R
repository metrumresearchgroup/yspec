

VALID_SPEC_NAMES <- c("type", "unit", "values", "decode",
                      "source", "comment",
                      "short", "long", "about", "dots",
                      "range", "longvalues",  "lookup", "axis")

check_spec_input_col <- function(x, col, env, not_allowed = NULL, ...) {
  err <- c()
  if(is.null(x)) return()
  # t0 <- !is.null(x)
  # if(!t0) {
  #   warning("No data for ", col)
  #   env$err[[col]] <- "no spec data was found"
  #   return(NULL)
  # }
  
  t1 <- all(names(x) %in% setdiff(VALID_SPEC_NAMES, not_allowed))
  if(!t1) {
    valid <- setdiff(VALID_SPEC_NAMES, not_allowed)
    inval <- setdiff(names(x), valid)
    inval <- paste(inval, collapse = ", ")
    err <- c(err,
             paste0(
               "invalid column field(s): ", inval
             ))
  }
  if(!is.list(x)) {
    err <- c(err, "item is not a list")
  }
  if(is.null(names(x))) {
    err <- c(err, "names not found")
  } else {
    if(any(nchar(names(x))==0)) {
      err <- c(err, "problem with names")
    }
  }
  env$err[[col]] <- err
}

check_spec_input <- function(x, .fun = check_spec_input_col,
                             context = "spec", ...) {
  err <- check_for_err(x, .fun)
  if(length(err)==0) return(invisible(NULL))
  file <- basename(get_meta(x)[["spec_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " input data\n", file, "\n", err)
}


check_this_col <- function(x,col,env,...) {
  err <- c()
  if(.has("values",x)) {
    if(any(x[["values"]] == "<yspec-null>")) {
      err <- c(err, "values field includes NULLs")  
    }
  }
  if(.has("values",x) & .has("range",x)) {
    err <- c(err, "column has both values and range")
  }
  if(.has("decode",x)) {
    if(length(x[["values"]]) != length(x[["decode"]])) {
      err <- c(
        err,
        "the length of values is not equal to the length of decode"
      )
    }
  }
  env$err[[col]] <- err
}

check_spec_cols <- function(x, context = "column") {
  err <- check_for_err(x, check_this_col)
  if(length(err)==0) return(invisible(NULL))
  file <- basename(get_meta(x)[["spec_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " data\n", file, "\n", err)
}

check_for_err <- function(x, .fun, ...) {
  env <- new.env()
  env$err <- set_names(vector("list", length(x)),names(x))
  walk2(x, names(x), .fun, env = env, ...)
  err <- discard(as.list(env)$err, is.null)
  if(length(err)==0) return(character(0))
  err <- imap_chr(err, .f = function(msg, col) {
    msg <- paste0("   - ", msg, collapse = "\n")
    paste0(" column: ",  col, "\n",
           msg, "\n", collapse = "\n")
  })
  err
}

capture_file_info <- function(x,file,where = "SETUP__") {
  x[[where]][["spec_file"]] <- file
  x[[where]][["spec_path"]] <- normalizePath(dirname(file),mustWork=FALSE)
  x
}

##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @param data_path optional path to data sets
##' @param data_stem optional alternate stem for data files
##' @param ... other arguments to update `SETUP__`
##' @export
ys_load <- function(file, ...) {
  x <- ys_load_file(file,...)
  unpack_spec(x)
}

##' @rdname ys_load
##' @export
ys_load_file <- function(file, data_path = NULL, data_stem = NULL, ...) {
  file <- normalizePath(file, mustWork = FALSE)
  x <- try_yaml(file)
  x <- capture_file_info(x,file)
  incoming <- list(...)
  incoming[["data_path"]] <- data_path
  incoming[["data_stem"]] <- data_stem
  unpack_meta(x, to_update = incoming, ...)
}

##' @rdname ys_load
##' @export
load_spec <- function(...) ys_load(...)

unpack_spec <- function(x) {
  
  check_spec_input(x)
  
  # defaults
  x[] <- imap(x,.f=col_initialize)
  
  # for looking up column data
  lookup <- ys_get_lookup(x)
  
  if(length(lookup) > 0) {
    x[] <- map_if(
      .x = x,
      .p = ~.x$do_lookup,
      .f = merge_lookup_column,
      lookup = lookup, 
      file = get_meta(x)[["spec_file"]]
    )
  }
  x[] <- map(x, unpack_col)
  check_spec_cols(x)
  structure(x, class = "yspec")
}

unpack_meta <- function(x,to_update) {
  meta <- list()
  metai <- names(x) == "SETUP__"
  if(any(metai)) {
    meta <- as.list(x[[which(metai)]])
    x <- x[!metai]
  }
  
  if(.no("description", meta)) {
    meta[["description"]] <- "[data set description]"  
  }
  if(exists("lookup_file", meta)) {
    assert_that(is.character(meta[["lookup_file"]]))
    meta[["lookup_file"]] <- file.path(meta[["spec_path"]],meta[["lookup_file"]])
    meta[["lookup_file"]] <- normalizePath(meta[["lookup_file"]],mustWork = FALSE)
  }
  if(.no("name", meta)) {
    meta[["name"]] <- basename(meta[["spec_file"]])
    meta[["name"]] <- tools::file_path_sans_ext(meta[["name"]])
  }
  if(.no("data_stem", meta)) {
    meta[["data_stem"]] <- meta[["name"]]
  }
  if(.no("data_path", meta)) {
    meta[["data_path"]] <- "../data/derived"
  }
  if(is.null(meta[["primary_key"]])) {
    meta[["primary_key"]] <- character(0)
  } else {
    assert_that(is.character(meta[["primary_key"]]))
    found_keys <- all(
      is.element(
        meta[["primary_key"]],
        names(x)
      ))
    if(!found_keys) {
      err_file(meta[["spec_file"]], "Invalid primary key.")
    }
  }
  meta <- update_list(meta,to_update)
  spec_validate_meta(meta)
  structure(x, meta = meta)
}


unpack_about <- function(x) {
  if(!exists("about",x)) {
    x[["about"]] <- c(x[["short"]],NA)
  }
  about <- x[["about"]][1:2]
  x[["about"]] <- NULL
  x[["short"]] <- as.character(about[1])
  if(!is.na(about[2])) {
    x[["unit"]] <- as.character(about[2])
  }
  x
}

unpack_col <- function(x) {
  
  if(identical(x,NULL)) {
    x <- list(short = x[["col"]], lookup=TRUE)
  }
  
  if(.no("short",x)) {
    x[["short"]] <- x[["col"]]
  }
  if(.no("type", x)) {
    x[["type"]] <- "numeric"
  }
  x <- unpack_about(x)
  x$continuous <- .has("range",x)
  if(x$continuous) {
    x$range <- unlist(x$range, use.names=FALSE)
  }
  x$discrete <- .has("values",x)
  if(.no("longvalues",x)) {
    x[["longvalues"]] <- FALSE
  }
  if(x$discrete) {
    if(!.has("decode",x)) {
      x$decode <- names(x$values)
    }
    x$values <- sapply(x$values, sub_null, USE.NAMES=FALSE)
    if(is.character(x$values)) x$type <- "character"
  }
  structure(x, class = "ycol")
}

sub_null <- function(x) {
  ifelse(is.null(x), "<yspec-null>", x)
}

col_initialize <- function(x, name) {
  
  if(is.null(x)) x <- list(lookup = TRUE)
  
  x[["col"]] <- name
  x[["do_lookup"]] <- NULL
  
  if(is.character(x[["lookup"]])) {
    x[["do_lookup"]] <- TRUE
  }
  
  if(is.logical(x[["lookup"]])) {
    x[["do_lookup"]] <- x[["lookup"]]
    if(x[["do_lookup"]]) {
      x[["lookup"]] <- name
    } else {
      x[["lookup"]] <- "<none>"
    }
  }
  
  if(.no("do_lookup",x)) {
    x[["do_lookup"]] <- FALSE
    x[["lookup"]] <- "<none>"
  }
  
  x
}

##' Load SETUP__ block from a file
##' 
##' @param file yaml file name
##' 
##' @export
ys_load_meta <- function(file) {
  get_meta(ys_load_file(file))
}

##' Load a specification file, guessing the type
##' 
##' @param file a yaml file name
##' @param ... arguments passed to [ys_load] or [ys_load_proj]
##' 
##' @examples
##' 
##' class(load_spec_any(file_spec_ex()))
##' 
##' class(load_spec_any(file_proj_ex()))
##' 
##' @export
load_spec_any <- function(file,...) {
  file <- normalizePath(file,mustWork=FALSE)
  if(is_yproj_file(file)) {
    return(ys_load_proj(file,...))
  }
  return(ys_load(file,...))
}
