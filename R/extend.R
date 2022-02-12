
#' Extend a yspec object
#' 
#' Use this function to read and join another spec file to an existing 
#' `yspec` object. The additional spec file can contain additional columns
#' that might be added to the data set on an ad-hoc basis or could include
#' modeling outputs (e.g. `IPRED`).
#' 
#' @details
#' The extension `yspec` object cannot contain any columns in common with the 
#' primary `yspec` object. 
#' 
#' @param x A `yspec` object. 
#' @param file The path to a yaml specification file to load and join to `x`. 
#' 
#' @examples
#' extension_file <- system.file("spec", "nm-extension.yml")
#' 
#' spec <- yys_help$spec() 
#' spec <- ys_extend(spec, extension_file)
#' tail(spec)
#' 
#' @md
#' @export
ys_extend <- function(x, file) {
  if(!file.exists(file)) {
    stop("Extension file does not exist: ", file)
  }
  extension <- ys_load(file, verbose = FALSE, extend = FALSE)
  if(any(names(extension) %in% names(x))) {
    message <- "\nNames in extension cannot also exist in the base spec:"
    common <- intersect(names(extension), names(x))
    common <- paste0("--| ", common, " was found in common\n")
    all <- c(message, common)
    all <- paste0(all, "\n")
    stop(all, call. = FALSE)  
  }
  c(x, extension)    
}

ys_load_extend <- function(x) {
  extend_file <- maybe_pull_meta(x, "extend_file")
  if(is.null(extend_file)) {
    stop(
      "There was no `extend_file` specified in the `SETUP__` block; ", 
      "cannot extend this yspec object."
    )
  }
  if(!(is.character(extend_file) && length(extend_file)==1)) {
    stop("`extend_file` must be a single string.")  
  }
  extend_file <- file.path(pull_meta(x, "spec_path"), extend_file)
  ys_extend(x, extend_file)
}
