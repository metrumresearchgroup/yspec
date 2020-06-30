
##' Create build location for branded documents
##' 
##' The idea is to create a build directory such that, when 
##' you render 
##' 
##' 
##' @param root directory where the build will take place
##' 
##' @details The following files will be copied into the 
##' build directory: `_output.yml`, `mrgtemplate.tex`, 
##' `header.tex`, `mrglogo.pdf`.
##' 
##' @return The path to the build directory.
##' 
##' @examples
##' 
##' ans <- mrgtemplate()
##' 
##' ans
##' 
##' list.files(ans)
##' @md
##' @export
mrgtemplate <- function(root = tempdir()) {
  dir <- file.path(root, "mrgtemplate")
  if(!dir.exists(dir)) dir.create(dir)
  dir <- normalPath(dir)
  templ_dir <- system.file(
    "mrgtemplate", package = "yspec"
  )
  files <- list.files(templ_dir, full.names=TRUE)
  foo <- file.copy(files, dir, overwrite = TRUE, recursive = TRUE)
  dir
}
