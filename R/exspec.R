##' Generate an example specification object
##'
##' @param file an example spec file name
##'
##' @examples
##'
##' load_spec_ex()
##' load_proj_ex()
##' file_spec_ex()
##' file_proj_ex()
##'
##' @export
load_spec_ex <- function(file = "spec.yml") {
  spec <- file_spec_ex(file)
  load_spec(spec)
}

##' @rdname load_spec_ex
##' @export
load_proj_ex <- function(file = "project.yml") {
  file <- file_proj_ex(file)
  load_spec_proj(file)
}

##' @rdname load_spec_ex
##' @export
file_spec_ex <- function(file = "spec.yml") {
  system.file("spec", file, package = "yspec")
}

##' @rdname load_spec_ex
##' @export
file_proj_ex <- function(file = "project.yml") {
  system.file("spec", file, package = "yspec")
}

# test_spec <- function(x) {
#   ys_load(
#     system.file(
#       "spec", "testthat", 
#       x, 
#       package = "yspec"
#     )
#   )
# }
