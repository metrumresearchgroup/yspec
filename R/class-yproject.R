
is_yproj <- function(x) {
  inherits(x, "yproj")
}

##' Load a project-wide spec file
##'
##' @param file file name
##'
##' @export
load_spec_proj <- function(file) {
  x <- try_yaml(file)
  x[["SETUP__"]][["yml_file"]] <- file
  x[["SETUP__"]][["path"]] <- path <- dirname(file)

  x <- unpack_meta(x)

  files <- names(x)

  for(i in seq_along(x)) {
    if(.no("description",x[[i]])) {
     .stop("no description found for ", files[i])
    }
    if(i==1) {
      if(.no("data_path",x[[i]])) {
        .stop("the first entry must specify the data_path")
      }
      next
    }
    x[[i]] <- combine_list(x[[i-1]], x[[i]])
  }

  x <- imap(x, function(x,y) {
    x[["spec_path"]] <- path
    x[["name"]] <- y
    if(.no("spec", x)) {
      x[["spec"]] <- paste0(y, ".yml")
    }
    if(.no("source", x)) {
      x[["source"]] <- paste0(y,".xpt")
    }
    if(.no("data_path",x)) {
      x[["data_path"]] <- get_meta(x)[["path"]]
    }
    x[["file"]] <- normalizePath(
      file.path(x[["spec_path"]],x[["spec"]])
    )
    x
  })
  structure(x, class = "yproj")
}

##' @export
print.yproj <- function(x,i=0,...) {
  names <- map_chr(x, "name")
  specs <- map_chr(x, "spec")
  ans <- data.frame(name = names, spec = specs)
  print.data.frame(ans, row.names = FALSE,
                   right = FALSE)
}

##' @export
##' @method as.list yproj
as.list.yproj <- function(x, ...) {
  unclass(x)
}
