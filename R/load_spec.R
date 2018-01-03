
##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @export
load_spec <- function(file) {
  x <- try_yaml(file)
  x[["SETUP__"]][["yml_file"]] <- file
  x[["SETUP__"]][["path"]] <- dirname(file)
  unpack_spec(x)
}

unpack_spec <- function(x) {

  # handle meta data
  meta <- list()
  metai <- names(x) == "SETUP__"
  if(any(metai)) {
    meta <- as.list(x[[which(metai)]])
    x <- x[!metai]
  }
  if(exists("lookup_file", meta)) {
    meta[["lookup_file"]] <- file.path(meta[["path"]],meta[["lookup_file"]])
    meta[["lookup_file"]] <- normalizePath(meta[["lookup_file"]])
  }
  if(is.null(meta[["primary_key"]])) {
    meta[["primary_key"]] <- character(0)
  } else {
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

  # for looking up column data
  x <- merge_lookup_spec(x,meta)
  x <- lapply(x, unpack_col)
  structure(x, class = "yspec", meta = meta)
}



load_lookup_spec <- function(x) {

  if(!exists("lookup_file",x)) {
    return(list())
  }
  ans <- list()
  for(look in x[["lookup_file"]]) {
    assert_that(file.exists(look))
    this <- try_yaml(look)
    ans <- combine_list(ans,this)
  }
  ans
}


merge_lookup_spec <- function(x, meta = meta(x)) {
  lookup <- load_lookup_spec(meta)
  cols <- names(x)

  tolook <- map_chr(x, "lookup", .default = NA)
  tolook[is.na(tolook)] <- cols[is.na(tolook)]
  has_lookup <- is.element(tolook,names(lookup))

  for(i in seq_along(x)) {

    def <- list(lookup = tolook[i],type = "numeric")
    xi <- x[[i]]
    if(is.null(xi)) {
      xi <- list()
    }
    xi <- combine_list(def, xi)
    if(has_lookup[i]) {
      xi <- combine_list(
        lookup[[xi[["lookup"]]]],
        xi
      )
    }
    xi[["col"]] <- cols[i]
    xi[["lookup"]] <- tolook[i]
    x[[i]] <- xi
  }
  return(x)
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




