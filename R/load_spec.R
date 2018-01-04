
##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @export
load_spec <- function(file) {
  file <- normalizePath(file, mustWork=TRUE)
  x <- try_yaml(file)
  x[["SETUP__"]][["yml_file"]] <- file
  x[["SETUP__"]][["path"]] <- dirname(file)
  unpack_spec(x)
}

unpack_spec <- function(x) {

  # handle meta data
  x <- unpack_meta(x)

  # defaults
  x[] <- imap(x,.f=set_defaults)

  # for looking up column data
  lookup <- load_lookup_spec(x)

  if(length(lookup) > 0) {
    x[] <- map(x,
               .f = merge_lookup_column,
               lookup = lookup
    )
  }

  # unpack the columns
  x[] <- map(x, unpack_col)

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
    meta[["lookup_file"]] <- normalizePath(meta[["lookup_file"]])
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
    this <- try_yaml(f)
    ans <- combine_list(ans,this)
  }
  ans
}


merge_lookup_column <- function(x,lookup) {
  name <- lookup.ycol(x)
  if(.has(name,lookup)) {
    x <- combine_list(
      lookup[[name]],
      x
    )
  }
  x
}

# Weight (kg)
make_axis_label <- function(x) {
  unit <- NULL
  if(has_unit(x)) {
    unit <- paste0(" (", x$unit,")")
  }
  x$axis_label <- paste0(x$short, unit)
  x
}

# WT//Weight (kg)
make_col_label <- function(x) {
  x$col_label <- paste0(x$col, "//", x$axis_label)
  x$plot_data <- c(x$col, x$axis_label)
  x
}

unpack_split_col <- function(x) {
  x <- unpack_about(x)
  if(!is.character(x[["when"]])) x[["when"]] <- NA
  x <- make_axis_label(x)
  x <- make_col_label(x)
  x
}

unpack_split <- function(x,col) {
  for(n in names(x)) {
    x[[n]][["col"]] <- col
    x[[n]][["name"]] <- n
    x[[n]] <- unpack_split_col(x[[n]])
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
  x <- unpack_about(x)
  x$continuous <- .has("range",x)
  if(x$continuous) {
    x$range <- unlist(x$range, use.names=FALSE)
  }
  x$discrete <- !x$continuous
  if(.no("longvalues",x)) {
    x[["longvalues"]] <- FALSE
  }
  if(x$discrete) {
    if(!.has("decode",x)) {
      x$decode <- names(x$values)
    }
    x$values <- unlist(x$values, use.names=FALSE)
  }
  x <- make_axis_label(x)
  x <- make_col_label(x)
  x$is_split <- FALSE
  if(exists("split", x)) {
    x$is_split <- TRUE
    x$split <- unpack_split(x$split,x$col)
  }
  if(.no("lookup",x)) {
    x[["lookup"]] <- x[["col"]]
  }
  x
}

set_defaults <- function(x, name,
                         def = list(type="numeric",
                                    col = name)) {
  if(is.null(x)) x <- list()
  x[["col"]] <- NULL
  structure(combine_list(def,x), class="ycol")
}


