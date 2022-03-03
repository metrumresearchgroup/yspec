
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
#' @param file The path to a yaml specification file to load and join to `x`; 
#' if `file` is not passed, the `yspec` object will be searched for the
#' `extend_file` attribute in `SETUP__:` and will fail if it is not found.
#' 
#' @examples
#' extension_file <- system.file("spec", "nm-extension.yml", package = "yspec")
#' 
#' spec <- ys_help$spec() 
#' spec2 <- ys_extend(spec, extension_file)
#' tail(spec2)
#' 
#' ys_extend(spec)
#' 
#' @md
#' @export
ys_extend <- function(x, file = ys_extend_file(x), report = FALSE) {
  if(!file.exists(file)) {
    stop("Extension file does not exist: ", file)
  }
  incoming <- length(x)
  extension <- ys_load(file, verbose = FALSE, extend = FALSE)
  ans <- ys_join(x, extension)
  if(isTRUE(report)) {
    msg <- glue("Note: added {length(ans) - length(x)} columns by extension.")
    message(msg)
  }
  ans
}

ys_extend_file <- function(x) {
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
  extend_file
}
