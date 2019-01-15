##' Get column lookup source
##' 
##' @param x a yspec object
##' @export
ys_lookup_source <- function(x) {
  home <- basename(get_meta(x)[["spec_file"]])
  xx <- map_chr(x, "lookup_source", .default = home)
  data_frame(col = names(x), lookup_source = xx)
}

##' Generate lookup list
##' 
##' @param x a yspec object 
##' 
##' @examples
##' 
##' spec <- load_spec_ex("DEM104101F_PK.yml")
##' ys_get_lookup(spec)
##' 
##' 
##' @export
ys_get_lookup <- function(x,syst = TRUE) {
  files <- get_lookup_files(x)
  if(any(basename(files)=="skip_ysdb_internal")) {
    files <- files[basename(files) != "skip_ysdb_internal"]
    syst <- FALSE
  }
  if(syst) files <- c(files,lookup_ysdb_file())
  if(length(files)==0) {
    return(list())
  }
  ans <- list()
  files <- rev(files)
  for(.file in files) {
    this <- ys_load_file(.file)
    check_spec_input(this, context = "lookup spec", not_allowed = "lookup")
    this <- map(this, function(x) {
      x[["lookup_source"]] <- basename(.file);
      x
    })
    ans <- combine_list(ans,this)
  }
  ans
}

get_lookup_files <- function(x) {
  ans <- get_meta(x)[["lookup_file"]]
  if(is.character(ans)) {
    return(ans)
  }
  return(character(0))
}

merge_lookup_column <- function(x,lookup,file) {
  lookup_name <- x[["lookup"]]
  if(.has(lookup_name,lookup)) {
    x <- combine_list(lookup[[lookup_name]],x)
  } else {
    warning(
      "Did not find lookup data for ", 
      lookup_name, ", file: ", basename(file), call.=FALSE
    )
  }
  x
}

lookup_ysdb_file <- function(do = TRUE) {
  file <- system.file("internal", "ysdb_internal.yml", package = "yspec")
}

