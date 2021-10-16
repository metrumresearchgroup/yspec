
##' Load a project-wide spec file
##'
##' @param file file name
##' @param ... used for alias only
##'
##' @examples
##'
##' file <- file_proj_ex()
##'
##' spec <- ys_load_proj(file)
##'
##' spec
##' @include utils.R
##' @export
ys_load_proj <- function(file) {
  
  if(!is_yproj_file(file)) {
    file <- paste0("file: ", file, "\n")
    stop(file, "Does not appear to be a yspec project file.", call.=FALSE)
  }
  
  x <- try_yaml(file)
  x <- capture_file_info(x,file,where="YPROJ__")
  
  x <- unpack_meta_yproj(x)
  meta <- get_meta(x)
  
  path <- meta[["spec_path"]]
  
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
  
  x[] <- imap(x, function(x,stem) {
    x <- combine_list(defaults,x)
    x[["name"]] <- stem
    add_path <- FALSE  
    if(.no("data_file", x)) {
      x[["data_file"]] <- paste0(x[["data_stem"]], ".xpt")
    }
    if(.no("data_path",x)) {
      x[["data_path"]] <- path
    }
    x
  })
  structure(x, class = "yproj", meta = meta)
}

##' @rdname ys_load_proj
##' @export
load_spec_proj<- function(...) {
  ys_load_proj(...)  
}

##' @export
print.yproj <- function(x,i=0,...) {
  met <- get_meta(x)
  names <- map_chr(x, "name")
  desc <- map_chr(x, "description")
  data_stem <- map_chr(x, "data_stem")
  ans <- data.frame(name = names, description = desc, data_stem = data_stem)
  cat("projectnumber: ", met[["projectnumber"]], "\n")
  cat("sponsor:       ", met[["sponsor"]], "\n")
  cat("--------------------------------------------\n")
  cat("datafiles: \n")
  print.data.frame(ans, row.names = FALSE, right = FALSE)
}

##' @export
##' @method as.list yproj
as.list.yproj <- function(x, ...) {
  unclass(x)
}

assemble_proj_info <- function(x) {
  met <- get_meta(x)
  description <- met[["description"]]
  if(is.null(description)) {
    stop("A description must be supplied in the SETUP__ front matter")  
  }
  file <- basename(met[["spec_file"]])
  name <- met[["name"]]
  data_path <- met[["data_path"]]
  if(is.null(data_path)) {
    data_path <- "../data/derived"
  }
  
  spec_file <- met[["spec_file"]]
  meta <- list(
    data_path = data_path,
    sponsor = met[["sponsor"]], 
    projectnumber = met[["projectnumber"]]
  )
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
##' @param dots used to update `SETUP__` block data items
##' @param sponsor optional project sponsor
##' @param projectnumber optional project number in `...`
##' @param keep_spec_data if `TRUE`, then the specification data is saved in the 
##' meta space of the project object
##' @return an object of class yproj
##' 
##' @details
##' Note that `sponsor` and `projectnumber` can be updated upon creating the 
##' project object.  Other document-specific attributes (like **title**, 
##' **author**, **date**) can be specified through [ys_document].
##' 
##' @examples
##' where <- system.file("spec", package = "yspec")
##' 
##' files <- file.path(where, c("DEM104101F_PK.yml", "DEM104101F_AE.yml"))
##' 
##' proj <- ys_project(ys_load(files[1]),ys_load(files[2]), sponsor="CompanyX")
##' 
##' proj <- ys_project_file(files[1], files[2], dots = list(projectnumber = "ZZZzzz"))
##' 
##' proj
##' 
##' @md
##' @export
ys_project <- function(..., output = tempfile(fileext = ".yml"), 
                       data_path = NULL, dots = list(),
                       sponsor = "[sponsor]", 
                       projectnumber = "[projectnumber]", 
                       keep_spec_data = FALSE) {
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
  output <- normalPath(output, mustWork = FALSE)
  if(missing(sponsor)) {
    if(.has("sponsor", meta)) {
      if(nchar(meta[["sponsor"]][[1]]) > 0) {
        sponsor <- meta[["sponsor"]][1]  
      }
    }
  }
  if(missing(projectnumber)) {
    if(.has("projectnumber", meta)) {
      if(nchar(meta[["projectnumber"]][[1]]) > 0) {
        projectnumber <- meta[["projectnumber"]][1]
      } 
    }
  }
  meta <- list(
    sponsor=sponsor, 
    projectnumber=projectnumber, 
    spec_file = output,
    spec_path = dirname(output),
    path = dirname(output), 
    class = "yproj"
  )
  if(keep_spec_data) meta[["data"]] <- do.call(as_spec_list,lst)
  meta <- update_list(meta,dots)
  txt <- yaml::as.yaml(c(list(YPROJ__ = meta),proj))
  if(dirname(output)==tempdir()) {
    #loc <- paste0(basename(output), " in tempdir()")
    #message("Writing project file to:\n", loc)
  } else {
    message("Writing project file to:\n ", output)  
  }
  writeLines(
    con = output,
    c("# Generated by yspec::as_proj_spec; do not edit by hand",txt)
  )
  structure(proj, meta=meta, class="yproj")
}

##' @rdname ys_project
##' @export
ys_project_file <- function(..., output = tempfile(fileext=".yml"), 
                            where = NULL, dots = list()) {
  
  files <- list(...) %>% unlist()
  if(is.character(where)) {
    files <- file.path(where,files)
  }
  files <- normalPath(files)
  specs <- lapply(files, ys_load_file)
  dots[["output"]] <- output
  do.call(as_proj_spec, c(specs, dots))
}

#' @rdname ys_project
#' @export
as_proj_spec <- function(...) {
  ys_project(...)  
}

unpack_meta_yproj <- function(x,...) {
  meta <- list()
  metai <- names(x) == "YPROJ__"
  if(any(metai)) {
    meta <- as.list(x[[which(metai)]])
    x <- x[!metai]
  }
  if(.no("name", meta)) {
    meta[["name"]] <- basename(meta[["spec_file"]])
    meta[["name"]] <- tools::file_path_sans_ext(meta[["name"]])
  }
  if(.no("name", meta)) {
    meta[["name"]] <- basename(meta[["spec_file"]])
    meta[["name"]] <- tools::file_path_sans_ext(meta[["name"]])
  }
  if(.no("data_path", meta)) {
    meta[["data_path"]] <- "../data/derived"
  }
  updates <- list(...)
  meta <- merge.list(meta,updates)
  structure(x, meta = meta)
}

is_yproj_file <- function(x) {
  x <- normalPath(x,mustWork=FALSE)
  if(!file.exists(x)) return(FALSE)
  trimws(scan_yml(file=x,n=1))=="YPROJ__:"
}

is_yproj <- function(x) {
  inherits(x, "yproj")
}

##' @export
update.yproj <- function(object, projectnumber=NULL, sponsor=NULL, ...) {
  
  non_null <- c(projectnumber,sponsor)
  
  if(is.null(non_null)) return(object)
  
  m <- get_meta(object)
  
  if(is.character(projectnumber)) {
    m[["projectnumber"]] <- projectnumber  
  }
  
  if(is.character(sponsor)) {
    m[["sponsor"]] <- sponsor  
  }
  
  structure(object, meta = m)
  
}