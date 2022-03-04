
get_spec_control <- function(meta = NULL) {
  ans <- ys_control_defaults()
  if(is.null(meta)) return(ans)
  mo <- map(ans, mode)
  for(nn in names(ans)) {
    if(.has(nn, meta)) {
      assert_that(
        identical(mode(meta[[nn]]), mo[[nn]]),
        msg = glue("meta field {nn} has wrong type (required: {mo[[nn]]})")
      )
      ans[[nn]] <- meta[[nn]]  
    }
  }
  ans
}

check_spec_input_col <- function(x, col, env, not_allowed = NULL, control, ...) {
  err <- c()
  if(is.null(x)) return()
  t0 <- nchar(col) <= control[["max_nchar_col"]]
  if(!t0) {
    err <- c(
      err,
      paste0(
        "column name more than ",  
        control[["max_nchar_col"]], 
        " characters long"
      )
    )
  }
  # fields within namespaces have the following pattern: field.namespace
  # e.g unit.tex; we remove everything after the first `.` when checking for 
  # valid names; eventually I'd like to refactor the load / check process a bit
  # to handle this in a more natural way
  fields <- sub("\\..*$", "", names(x))
  t1 <- all(fields %in% setdiff(VALID_SPEC_NAMES, not_allowed))
  if(!t1) {
    valid <- setdiff(VALID_SPEC_NAMES, not_allowed)
    inval <- setdiff(fields, valid)
    inval <- paste(inval, collapse = ", ")
    err <- c(
      err,
      paste0(
        "invalid column field(s): ", inval
      )
    )
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
  err <- check_for_err(x, .fun, ...)
  if(length(err)==0) return(invisible(NULL))
  file <- basename(get_meta(x)[["spec_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " input data\n", file, "\n", err)
}


check_this_col <- function(x, col, env, control, ...) {
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
  
  if(length(x$unit) > 1) {
    err <- c(err, "the 'unit' field should not be more than length 1")  
  }
  if(length(x$type) > 1) {
    err <- c(err, "the 'type' field should not be more than length 1")  
  }
  if(length(x$short) > 1) {
    err <- c(err, "the 'short' field should not be more than length 1")  
  }
  if(length(x$label) > 1) {
    err <- c(err, "the 'label' field should not be more than length 1")  
  }
  if(sum(nchar(x$label)) > control[["max_nchar_label"]]) {
    nmax <- control[["max_nchar_label"]]
    msg <- "the 'label' field should not be longer than {nmax} characters"
    err <- c(err, as.character(glue(msg)))
  }
  if(isTRUE(env$require.label)) {
    if(!is.character(x$label)) {
      err <- c(err, "'label' is required for every column, but is missing")  
    }
  }
  if(sum(nchar(x$short)) > control[["max_nchar_short"]]) {
    nmax <- control[["max_nchar_short"]]
    msg <- "the 'short' field should not be longer than {nmax} characters"
    err <- c(err, as.character(glue(msg)))
  }
  if(! all(x[["type"]] %in% c("numeric", "character", "integer"))) {
    bad <- setdiff(x[["type"]],c("numeric", "character", "integer"))
    err <- c(
      err, 
      paste0("'type' must be 'numeric', 'character' or 'integer' ('", bad, "')")
    )
  }
  env$err[[col]] <- err
}

check_spec_cols <- function(x, context = "column", ...) {
  err <- check_for_err(x, check_this_col, ...)
  if(length(err)==0) return(invisible(NULL))
  file <- basename(get_meta(x)[["spec_file"]])
  file <- paste0("In file: ", file)
  .stop("invalid ", context, " data\n", file, "\n", err)
}

check_for_err <- function(x, .fun, ...) {
  env <- new.env()
  env$err <- set_names(vector("list", length(x)), names(x))
  env$require.label <- getOption("ys.require.label", FALSE)
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
  x[[where]][["spec_path"]] <- normalPath(dirname(file),mustWork=FALSE)
  x
}

##' Load a data specification file
##'
##' @param file Name of yaml file containing specification.
##' @param data_path Optional path to data sets.
##' @param data_stem Optional alternate stem for data files.
##' @param verbose Logical: print information to the console as the file
##' is processed.
##' @param ... Other arguments to update `SETUP__`.
##' 
##' @examples
##' 
##' sp <- ys_load(ys_help$file())
##' sp
##' 
##' sp <- ys_load(ys_help$file(), verbose = TRUE)
##' 
##' @md
##' @export
ys_load <- function(file, verbose = FALSE, ...) {
  # not using lifecycle yet; possibly in the future
  if(!is.null(getOption("ys.col.len", NULL))) {
    warning(
      "The option `ys.col.len` has been deprecated; please use the control\n", 
      "field `max_nchar_col` in SETUP__: instead of the option.", 
      call. = FALSE
    )
  }
  x <- ys_load_file(file, verbose = verbose, ...)
  x <- unpack_spec(x, verbose = verbose)
  x <- add_flags(x)
  set_namespace(x, "base")
}

##' @rdname ys_load
##' @export
ys_load_file <- function(file, data_path = NULL, data_stem = NULL, 
                         verbose = FALSE, ...) {
  if(!is.character(file)) {
    stop("'file' argument must have class character (not ", class(file)[1],")",call.=FALSE)  
  }
  file <- normalPath(file, mustWork = FALSE)
  if(verbose) verb("~ working on", basename(file))
  x <- try_yaml(file)
  x <- capture_file_info(x, file)
  incoming <- list(...)
  incoming[["data_path"]] <- data_path
  incoming[["data_stem"]] <- data_stem
  unpack_meta(x, to_update = incoming, verbose = verbose)
}

##' @rdname ys_load
##' @export
load_spec <- function(...) ys_load(...)

unpack_spec <- function(x, verbose = FALSE) {
  
  control <- pull_meta(x, "control")
  
  check_spec_input(x, control = control)
  
  # for looking up column data
  x[] <- imap(x, .f = col_initialize)
  
  lookup <- ys_get_lookup(x, verbose = verbose)
  
  if(length(lookup) > 0 & verbose) {
    verb(relapse(":",13), relapse(":",30))
  }
  
  x[] <- map_if(
    .x = x,
    .p = ~.x$do_lookup,
    .f = merge_lookup_column,
    lookup = lookup, 
    file = pull_meta(x, "spec_file"), 
    verbose = verbose
  )
  
  x[] <- map(x, unpack_col)
  
  m <- get_meta(x)
  m[["namespace"]] <- list_namespaces(x)
  x <- structure(x, meta = m)
  
  check_spec_cols(x, control = control)
  ans <- structure(x, class = "yspec")
  if(.has("import", get_meta(x))) {
    import <- ys_load(maybe_pull_meta(x, "import"))
    ans <- c(import, ans, .meta = get_meta(ans))
  }
  if(isTRUE(maybe_pull_meta(x, "character_last"))) {
    type <- map_chr(ans, "type")
    chr <- type == "character"
    comment <- names(ans) %in% maybe_pull_meta(x, "comment_col")
    ans <- c(ans[(!chr) | comment], ans[chr & (!comment)])
  }
  ans
}

unpack_meta <- function(x, to_update, verbose = FALSE, ...) {
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
    meta[["lookup_file"]] <- file.path(meta[["spec_path"]], meta[["lookup_file"]])
    meta[["lookup_file"]] <- normalPath(meta[["lookup_file"]], mustWork = FALSE)
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
      err_file(meta[["spec_file"]], "invalid primary key.")
    }
  }
  if(verbose) {
    verb("  description", meta[["description"]])
    if(.has("project", meta)) verb("  project", meta[["project"]])
    if(.has("sponsor", meta)) verb("  sponsor", meta[["sponsor"]])
  }
  meta <- update_list(meta, to_update)
  if(exists("import", meta)) {
    meta[["import"]] <- fs::path_abs(meta[["import"]], meta[["spec_path"]])  
  }
  spec_validate_meta(meta)
  meta[["flags"]] <- validate_flags(meta[["flags"]], valid = names(x))
  meta[["control"]] <- get_spec_control(meta)
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
  if(.has("source", x)) {
    x$source <- paste0(x$source, collapse = " ")  
  }
  if(.has("comment", x)) {
    x$comment <- paste0(x$comment, collapse = " ")  
  }
  if(.has("long", x)) {
    x$long <- paste0(x$long, collapse = " ")
  }
  x <- create_namespaces(x)
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
##' @param ... arguments passed to [ys_load()] or [ys_load_proj()]
##' 
##' @examples
##' 
##' class(load_spec_any(file_spec_ex()))
##' 
##' class(load_spec_any(file_proj_ex()))
##' 
##' @md
##' @export
load_spec_any <- function(file,...) {
  file <- normalPath(file,mustWork=FALSE)
  if(is_yproj_file(file)) {
    return(ys_load_proj(file,...))
  }
  return(ys_load(file,...))
}
