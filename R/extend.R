
#' Extend a yspec object
#' 
#' Use this function to read and join another spec file to an existing 
#' `yspec` object. The additional spec file can contain additional columns
#' that might be added to the data set on an ad-hoc basis or could include
#' modeling outputs (e.g. `IPRED`).
#' 
#' @details
#' The extension is accomplished using [ys_join()], so any columns in the 
#' extension spec that already exist in the primary spec are dropped. Use
#' [ys_select()] on the the primary spec to drop columns that might be in the
#' extension and that you want to retain in the result.
#' 
#' If there are no new columns added by extension, that indicates all columns 
#' in the extension spec already exist in primary spec. In this case, a 
#' warning will be generated. 
#' 
#' @param x A `yspec` object (the primary spec). 
#' @param file The path to a yaml specification file to load and join to `x`; 
#' if `file` is not passed, the `yspec` object will be searched for the
#' `extend_file` attribute in `SETUP__:` and will fail if it is not found.
#' @param silent Logical; if `TRUE`, issue message reporting the number of 
#' columns added via extension; the user will alternatively be warned if there
#' were no columns added. 
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
#' \dontrun{
#' # In case COL is in both the primary spec and the extension, but you want 
#' # to retain what is in the extension
#' spec %>% select(-COL) %>% ys_extend("extension.yml")   
#' }
#' 
#' @md
#' @export
ys_extend <- function(x, file = ys_extend_file(x), silent = FALSE) {
  if(!file.exists(file)) {
    stop("Extension file does not exist: ", file)
  }
  extension <- ys_load(file, verbose = FALSE)
  extension <- modify(extension, extended_from, file = basename(file))
  ans <- ys_join(x, extension)
  n_x <- length(x)
  n_ext <- length(extension)
  n_ans <- length(ans)
  if(isFALSE(silent)) {
    if(n_ans == n_x) {
      msg <- c(
        "The input spec object was not extended.", 
        i = glue("Length primary:   {n_x}"), 
        i = glue("Length extension: {n_ext}") 
      )
      warn(msg)
    } else {
      msg <- glue("Note: added {n_ans - n_x} columns by extension.")
      message(msg)    
    }
  }
  ans
}

extended_from <- function(x, file) {
  x[["extended_from"]] <- file
  x
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
