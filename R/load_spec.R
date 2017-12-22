
##' Load a data specification file
##'
##' @param file name of yaml file containing specification
##' @export
load_spec <- function(file) {
  x <- try(yaml::yaml.load_file(file))
  if(is_error(x)) {
    tryfile <- paste0("yaml::yaml.load_file(\"",file,"\")")
    stop(
      "failed to parse the file ",
      basename(file),
      "\n",
      "please try running ",
      tryfile,
      " and fix yaml code",
      call. = FALSE
    )
  }
  x[["SETUP__"]][["yml_file"]] <- file
  unpack_spec(x)
}

load_lookup_spec <- function(x) {
  if(!exists("lookup_file",x)) return(list())
  if(!file.exists(x$lookup_file)) {
    stop("Couldn't find lookup file", call. = FALSE)
  }
  ans <- try(yaml.load_file(x$lookup_file))
  if(is_error(ans)) {
    stop(
      "failed to parse the lookup file ",
      basename(file),
      call. = FALSE
    )
  }
  ans
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
    x[["about"]] <- c(x[["col"]],NA)
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
  x <- unpack_about(x)
  x$continuous <- .has(x,range)
  if(x$continuous) {
    x$range <- unlist(x$range, use.names=FALSE)
  }
  x$discrete <- !x$continuous
  if(.no(x,"longvalues")) x[["longvalues"]] <- FALSE
  if(x$discrete) {
    if(!.has(x,decode)) {
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
  x
}

unpack_spec <- function(x) {

  # handle meta data
  meta <- list()
  metai <- grepl("SETUP__", names(x), fixed = TRUE)
  if(any(metai)) {
    meta <- x[[which(metai)]]
    x <- x[!metai]
  }
  if(exists("lookup_file", meta)) {
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
      stop(
        "invalid primary key in ",
        basename(meta[["yml_file"]]),
        call. = FALSE
      )
    }
  }

  # for looking up column data
  lookup <- load_lookup_spec(meta)
  cols <- names(x)
  has_lookup <- is.element(cols,names(lookup))
  for(i in seq_along(x)) {
    def <- list(lookup = cols[i], type = "numeric")
    xi <- x[[i]]
    if(is.null(xi)) {
      xi <- list()
    }
    xi <- merge.list(def, xi, open = TRUE)
    if(has_lookup[i]) {
      xi <- merge.list(
        lookup[[xi[["lookup"]]]],
        xi,
        open = TRUE
      )
    }
    xi[["col"]] <- cols[i]
    xi <- unpack_col(xi)
    x[[i]] <- xi
  }
  structure(x, class = "yspec", meta = meta)
}


fetch_lookup <- function(xi,lookup) {

}
