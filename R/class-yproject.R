
is_yproj <- function(x) {
  inherits(x, "yproj")
}

##' Load a project-wide spec file
##'
##' @param file file name
##'
##' @examples
##'
##' file <- file_proj_ex()
##'
##' spec <- load_spec_proj(file)
##'
##' spec
##'
##' @export
load_spec_proj <- function(file) {
  
  x <- load_spec_file(file)
  
  meta <- get_meta(x)
  
  path <- meta[["path"]]
  
  defaults <- meta[names(meta) %in% c("data_path")]
  
  # no nulls
  nulls <- map_lgl(x, is.null)
  if(any(nulls)) {
    err <- paste0(names(x)[which(nulls)],collapse = ', ')
    .stop("empty entries in project file\n ", err)
  }
  
  # description is required
  desc <- map_chr(x, "description", .default = '.')
  if(any(desc=='.')) {
    err <- paste0(names(x)[which(is.na(desc))],collapse = ', ')
    .stop("entries in project file with no description\n ", err)
  }
  
  x[] <- imap(x, function(x,y) {
    x <- combine_list(defaults,x)
    
    x[["name"]] <- y
    
    add_path <- FALSE  
    if(.no("spec_path", x)) {
      x[["spec_path"]] <-  path 
      add_path <- TRUE
    } else {
      x[["spec_path"]] <- x[["path"]]  
    }
    if(.no("spec_file", x)) {
      x[["spec_file"]] <- paste0(y, ".yml")
    }
    if(.no("data_file", x)) {
      x[["data_file"]] <- paste0(y, ".xpt")
    }
    if(.no("data_path",x)) {
      x[["data_path"]] <- path
    }
    if(add_path) {
      x[["spec_file"]] <- normalizePath(
        file.path(x[["spec_path"]], x[["spec_file"]]),
        mustWork = FALSE
      )
    }
    x
  })
  structure(x, class = "yproj", meta = meta)
}

##' @export
print.yproj <- function(x,i=0,...) {
  met <- get_meta(x)
  names <- map_chr(x, "name")
  desc <- map_chr(x, "description")
  ans <- data.frame(name = names, description = desc)
  cat("projectnumber: ", met[["projectnumber"]], "\n")
  cat("sponsor:       ", met[["sponsor"]], "\n")
  cat("-----------------------------------\n")
  cat("datafiles: \n")
  print.data.frame(ans, row.names = FALSE, right = FALSE)
}

##' @export
##' @method as.list yproj
as.list.yproj <- function(x, ...) {
  unclass(x)
}

# applied to each specification in the project
# x is a spec object
# met is the meta data for that spec object

assemble_proj_info <- function(x) {
  met <- get_meta(x)
  description <- met[["description"]]
  if(is.null(description)) {
    stop("A description must be supplied in the SETUP__ front matter")  
  }
  file <- basename(met[["yml_file"]])
  name <- met[["name"]]
  data_path <- met[["data_path"]]
  if(is.null(data_path)) {
    data_path <- "../data/derived"
  }

  spec_file <- met[["yml_file"]]
  meta <- list(data_path = data_path)
  ans <- list(
    name = name, 
    description = description, 
    spec_path = dirname(spec_file), 
    spec_file = spec_file,
    data_path = data_path, 
    data_stem = met[["data_stem"]],
    meta = meta
  )
  ans[["csv_file"]] <- do.call(csv_file_name,ans)
  ans[["xpt_file"]] <- do.call(xpt_file_name,ans)
  ans
}

xpt_file_name <- function(data_path, data_stem, ext = ".xpt", ...) {
  file.path(data_path, paste0(data_stem,ext))
}

csv_file_name <- function(data_path, data_stem, ext = ".csv", ...) {
  file.path(data_path, paste0(data_stem,ext))
}


##' Create a project object from data spec objects
##' 
##' @param ... yspec objects or file names of yaml specification files
##' @param output the name and path where the project file is to be written
##' @param where directory containing the specification files if listed
##' @param data_path optional data path
##' @param sponsor optional project sponsor
##' @param projectnumber optional project number
##' in \code{...}
##' @return an object of class yproj
##' @export
as_proj_spec <- function(..., output=tempfile(fileext=".yml"), data_path = NULL, 
                         sponsor = "[sponsor]", projectnumber = "[projectnumber]") {
  lst <- list(...)
  proj <- map(lst, assemble_proj_info)
  meta <- map(proj, "meta")
  meta <- 
    transpose(meta) %>% 
    map(compact) %>% 
    map(flatten_chr) %>% 
    map(unique)
  proj <- make_null(proj, "meta")
  meta <- map(meta, function(x) {
    if(length(x)==0) x <- ''
    x
  })
  names(proj) <- map_chr(proj, "name")
  if(any(duplicated(names(proj)))) {
    dups <- names(proj)[duplicated(names(proj))]
    dups <- paste0("  ", dups)
    stop("Duplicated spec names:\n", paste0(dups,collapse = "\n"))
  }
  output <- normalizePath(output,mustWork=FALSE)
  if(missing(sponsor)) {
    if(.has("sponsor", meta)) {
      sponsor <- meta$sponsor[1]  
    }
  }
  if(missing(projectnumber)) {
    if(.has("projectnumber", meta)) {
      sponsor <- meta$projectnumber[1]  
    } 
  }
  meta <- list(
    sponsor=sponsor, 
    projectnumber=projectnumber, 
    yml_file = output, 
    proj_file = output,
    proj_path = dirname(output),
    path = dirname(output)
  )
  txt <- yaml::as.yaml(c(list(SETUP__ = meta),proj))
  if(dirname(output)==tempdir()) {
    loc <- paste0(basename(output), " in tempdir()")
    message("Writing project file to:\n", loc)
  } else {
    message("Writing project file to:\n ", output)  
  }
  writeLines(
    con = output,
    c("# Generated by yspec::as_proj_spec; do not edit by hand",txt)
  )
  structure(proj, meta=meta, class="yproj")
}

##' @rdname as_proj_spec
##' @export
as_proj_spec_file <- function(..., output = tempfile(fileext=".yml"), where = NULL) {
  
  files <- list(...) %>% unlist()
  if(is.character(where)) {
    files <- file.path(where,files)
  }
  files <- normalizePath(files)
  specs <- lapply(files, load_spec_file)
  do.call(as_proj_spec, c(specs, list(output=output)))
}

#' @rdname as_proj_spec
#' @export
ys_project <- function(...) {
  as_proj_spec(...)  
}


