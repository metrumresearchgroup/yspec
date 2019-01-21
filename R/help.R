
##' View example workflow assets
##' 
##' This function copies files out of the yspec install directory and into 
##' the directory of your choosing.  
##' 
##' @param output the directory where assets should be copied
##' @param overwrite passed to [file.copy]
##' 
##' @details
##' - `analysis1.yml`: an example yaml file
##' - `analysis1.csv`: data set that works with `analysis1.yml`
##' - `ysdb_internal.yml`: the internal column lookup data base
##' - `ys_get_assets.md`: a listing of assets that is exported
##' 
##' @examples
##' ys_get_assets("yspec_stuff")
##' 
##' 
##' @md
##' @export
ys_get_assets <- function(output,overwrite=FALSE) {
  output <- normalizePath(output, mustWork=FALSE)
  if(!overwrite) {
    if(dir.exists(output)) {
      stop(
        "The output directory already exists; remove it or use ", 
        "overwrite=TRUE", call.=FALSE
      )
    }
  }
  if(!dir.exists(output)) dir.create(output)
  cp <- function(...) {
    file.copy(system.file(..., package = "yspec"), output, overwrite=overwrite)  
  }
  a <- cp("spec",     "analysis1.yml")
  b <- cp("internal", "ysdb_internal.yml")
  c <- cp("internal", "ys_get_assets.md")
  d <- cp("internal", "analysis1.csv")
  return(a&b&c&d)  
}

##' View example specification file
##' 
##' @param file the example specification file to view
##' 
##' @details
##' The specification file will be copied out of the yspec install 
##' location and into a temporary file.  That file will be opened as a regular
##' file in case `rstudioapi` is installed.  Otherwise, the file is opened
##' for viewing using [file.show].
##' 
##' @examples
##' \dontrun{
##' ys_view()
##' }
##' 
##' @export
ys_view <- function(file = c("analysis1.yml", "ysdb_internal.yml")) {
  file <- match.arg(file)
  file <- system.file("internal", file, package = "yspec")
  tmp <- tempfile(pattern = "example", fileext=".yml")
  file.copy(file,tmp)
  if(requireNamespace("rstudioapi")) {
    rstudioapi::navigateToFile(tmp)
  } else {
    file.show(file)  
  }
}


