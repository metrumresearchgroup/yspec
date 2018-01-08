
is_yproj <- function(x) {
  inherits(x, "yproj")
}

##' Load a project-wide spec file
##'
##' @param file file name
##'
##' @export
load_spec_proj <- function(file) {

  x <- load_spec_file(file)

  defaults <- get_meta(x)
  path <- defaults[["path"]]

  defaults <- defaults[names(defaults) %in% c("data_path")]

  nulls <- map_lgl(x, is.null)
  if(any(nulls)) {
    err <- paste0(names(x)[which(nulls)],collapse = ', ')
    .stop("empty entries in project file\n ", err)
  }

  desc <- map_chr(x, "description", .default = as.character(NA))
  if(any(is.na(desc))) {
    err <- paste0(names(x)[which(is.na(desc))],collapse = ', ')
    .stop("entries in project file with no description\n ", err)
  }

  x[] <- imap(x, function(x,y) {
    x <- combine_list(defaults,x)
    x[["spec_path"]] <- path
    x[["name"]] <- y
    if(.no("spec_file", x)) {
      x[["spec_file"]] <- paste0(y, ".yml")
    }
    if(.no("data_file", x)) {
      x[["data_file"]] <- paste0(y,".xpt")
    }
    if(.no("data_path",x)) {
      x[["data_path"]] <- path
    }
    x[["spec_file"]] <- normalizePath(
      file.path(x[["spec_path"]],x[["spec_file"]])
    )
    x
  })

  structure(x, class = "yproj", meta = get_meta(x))
}

##' @export
print.yproj <- function(x,i=0,...) {
  names <- map_chr(x, "name")
  desc <- map_chr(x, "description")
  ans <- data.frame(name = names, description = desc)
  print.data.frame(ans, row.names = FALSE,
                   right = FALSE)
}

##' @export
##' @method as.list yproj
as.list.yproj <- function(x, ...) {
  unclass(x)
}