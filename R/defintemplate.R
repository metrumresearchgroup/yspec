
#' Create build location for templated define documents
#' 
#' @param root directory where the build will take place
#' 
#' @details The following files will be copied into the 
#' build directory: `_output.yml`, `definetemplate.tex`, 
#' `header.tex`. 
#' 
#' @return The path to the build directory.
#' 
#' @examples
#' 
#' ans <- definetemplate()
#' 
#' ans
#' 
#' list.files(ans)
#' @md
#' @export
definetemplate <- function(root = tempdir()) {
  dir <- file.path(root, "definetemplate")
  if(!dir.exists(dir)) dir.create(dir)
  dir <- normalPath(dir)
  templ_dir <- system.file(
    "definetemplate", 
    package = "yspec"
  )
  files <- list.files(templ_dir, full.names=TRUE)
  foo <- file.copy(files, dir, overwrite = TRUE, recursive = TRUE)
  invisible(dir)
}

#' @param ... arguments passed to [definetemplate()] when [mrgtemplate()] is 
#' called
#' @rdname definetemplate
#' @export
mrgtemplate <- function(...) {
  warning("mrgtemplate() has been deprecated; using definetemplate() instead")
  definetemplate(...)
}
