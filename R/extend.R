
ys_load_extend <- function(x) {
  extend_file <- maybe_pull_meta(x, "extend_file")
  if(is.null(extend_file)) {
    stop(
      "There was no `extend_file` specified in the `SETUP__` block; ", 
      "cannot extend this yspec object."
    )
  }
  if(!is.character(extend_file)) {
    stop("`extend_file` must have type `character`.")  
  }
  extend_file <- file.path(pull_meta(x, "spec_path"), extend_file)
  if(!file.exists(extend_file)) {
    stop("`extend_file` does not exist: ", extend_file)
  }
  extension <- ys_load(extend_file, verbose = FALSE, extend = FALSE)
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

