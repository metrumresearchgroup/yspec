#' Get column lookup source
#' 
#' @param x a yspec object
#' @export
ys_lookup_source <- function(x) {
  home <- basename(get_meta(x)[["spec_file"]])
  xx <- map_chr(x, "lookup_source", .default = home)
  tibble(col = names(x), lookup_source = xx)
}

#' Generate lookup list
#' 
#' @param x a yspec object 
#' @param verbose `logical`; print information to the console as the file
#' is processed
#' 
#' @examples
#' 
#' spec <- load_spec_ex("DEM104101F_PK.yml")
#' ys_get_lookup(spec)
#' 
#' @md
#' @export
ys_get_lookup <- function(x,verbose=FALSE) {
  files <- get_lookup_files(x)
  m <- get_meta(x)
  if(isTRUE(m[["use_internal_db"]])) {
    files <-  c(files, lookup_ysdb_file())
  }
  if(length(files)==0) return(list())
  if(verbose) {
    walk(basename(files), verb, left = "  lookup file")
  }
  ans <- list()
  files <- rev(files)
  for(.file in files) {
    this <- ys_load_file(.file, verbose = verbose)
    control <- get_spec_control(get_meta(this))
    check_spec_input(
      this, 
      context = "lookup spec", 
      not_allowed = "lookup", 
      control = control
    )
    # need to set col in order to run add_flags 
    this[] <- imap(this, ~ {.x[["col"]] = .y; .x})
    this <- add_flags(this)
    this[] <- map(this, function(x) {
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

merge_lookup_column <- function(x, lookup, file, verbose = FALSE) {
  lookup_name <- x[["lookup"]]
  if(.has(lookup_name, lookup)) {
    if(verbose) {
      a <- lookup[[lookup_name]][["lookup_source"]]
      b <- " ---> "
      c <- crayon::bold(crayon::blue(lookup_name)) 
      verb("~ looking up", c(a, b, c))
    }
    # special handling for dots
    lookup_dots <- lookup[[lookup_name]][["dots"]]
    lookup[[lookup_name]][["dots"]] <- NULL
    x <- combine_list(lookup[[lookup_name]], x)
    x <- merge_dots(x, lookup_dots)
  } else {
    warning(
      "did not find lookup data for ", 
      lookup_name, ", file: ", basename(file), call. = FALSE
    )
  }
  x
}

lookup_ysdb_file <- function(do = TRUE) {
  system.file("internal", "ysdb_internal.yml", package = "yspec")
}

merge_dots <- function(x, dots) {
  # nothing to merge
  if(!is.list(dots)) {
    return(x)  
  }
  # dots exist in lookup but not col - replace
  if(!is.list(x[["dots"]])) {
    x[["dots"]] <- dots
    return(x)
  }
  # dots exist in lookup and in col - merge
  x[["dots"]] <- combine_list(dots, x[["dots"]])
  x
}
