
load_lookup_spec <- function(x,syst = TRUE) {
  files <- get_lookup_files(x)
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

merge_lookup_column <- function(x,lookup) {
  msg <- getOption("yspec.lookup.message", FALSE)
  lookup_name <- x[["lookup"]]
  if(.has(lookup_name,lookup)) {
    if(msg) {
      message(
        "Merging ", 
        formatC(lookup_name, width = 10, flag = "-"),
        " from ", 
        lookup[[lookup_name]][["lookup_source"]]
      )
    }
    x <- combine_list(lookup[[lookup_name]],x)
  } else {
    warning(
      "Could not find lookup data for column ", 
      lookup_name, ".", call.=FALSE
    )
  }
  x
}



lookup_ysdb_file <- function(do = TRUE) {
  file <- system.file("internal", "ysdb_internal.yml", package = "yspec")
}
