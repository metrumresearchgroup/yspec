

VALID_SPEC_NAMES <- c("type", "unit", "values", "decode",
                      "source", "comment",
                      "short", "long", "about", "dots",
                      "range", "longvalues",  "lookup", "axis")

check_spec_input_col <- function(x, col, env, not_allowed = NULL, ...) {
  err <- c()
  t0 <- !is.null(x)
  if(!t0) {
    env$err[[col]] <- "no spec data was found"
    return(NULL)
  }

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
  file <- basename(get_meta(x)[["yml_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " input data\n", file, "\n", err)
}


check_this_col <- function(x,col,env,...) {
  err <- c()
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
  file <- basename(get_meta(x)[["yml_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " data\n", file, "\n", err)
}

check_for_err <- function(x, .fun, ...) {
  env <- new.env()
  env$err <- set_names(vector("list", length(x)),names(x))
  walk2(x, seq_along(x), .fun, env = env, ...)
  err <- discard(as.list(env)$err, is.null)
  if(length(err)==0) return(character(0))
  err <- imap_chr(err, .f = function(msg, col) {
    msg <- paste0("   - ", msg, collapse = "\n")
    paste0(" column: ",  col, "\n",
           msg, "\n", collapse = "\n")
  })
  err
}

# workhorse load and prep function
# used to load spec, lookup, and project files
load_spec_file <- function(file) {
  file <- normalizePath(file, mustWork = FALSE)
  x <- try_yaml(file)
  x <- capture_file_info(x,file)
  unpack_meta(x)
}

capture_file_info <- function(x,file,where = "SETUP__") {
  x[[where]][["yml_file"]] <- file
  x[[where]][["path"]] <- dirname(file)
  x
}

##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @export
load_spec <- function(file) {
  x <- load_spec_file(file)
  unpack_spec(x)
}

unpack_spec <- function(x) {

  check_spec_input(x)

  # defaults
  x[] <- imap(x,.f=col_initialize)

  # for looking up column data
  lookup <- load_lookup_spec(x)

  if(length(lookup) > 0) {
    x[] <- map_if(.x = x,
                  .p = ~.x$do_lookup,
                  .f = merge_lookup_column,
                  lookup = lookup
    )
  }

  # unpack the columns
  x[] <- map(x, unpack_col)

  check_spec_cols(x)

  # return `yspec`
  structure(x, class = "yspec")
}

unpack_meta <- function(x) {
  meta <- list()
  metai <- names(x) == "SETUP__"
  if(any(metai)) {
    meta <- as.list(x[[which(metai)]])
    x <- x[!metai]
  }
  if(exists("lookup_file", meta)) {
    assert_that(is.character(meta[["lookup_file"]]))
    meta[["lookup_file"]] <- file.path(meta[["path"]],meta[["lookup_file"]])
    meta[["lookup_file"]] <- normalizePath(meta[["lookup_file"]],mustWork = FALSE)
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
      .stop(
        "invalid primary key in ",
        basename(meta[["yml_file"]])
      )
    }
  }
  structure(x, meta = meta)
}

load_lookup_spec <- function(x) {
  files <- get_lookup_files(x)
  if(length(files)==0) {
    return(list())
  }
  ans <- list()
  for(f in files) {
    this <- load_spec_file(f)
    check_spec_input(this, context = "lookup spec", not_allowed = "lookup")
    ans <- combine_list(ans,this)
  }
  ans
}


merge_lookup_column <- function(x,lookup) {

  lookup_name <- x[["lookup"]]

  if(.has(lookup_name,lookup)) {
    x <- combine_list(
      lookup[[lookup_name]],
      x
    )
  } else {
    warning("could not find lookup for column ", lookup_name)
  }
  x
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
    x$values <- unlist(x$values, use.names=FALSE)
  }
  structure(x, class = "ycol")
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


