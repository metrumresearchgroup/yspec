##' Generate an example specification object
##'
##' @param file an example spec file name
##'
##' @export
##'
load_spec_ex <- function(file = "spec.yml") {
 spec <- spec_ex_file(file)
 load_spec(spec)
}

##' @rdname load_spec_ex
##' @export
load_proj_ex <- function(file = "project.yml") {
  file <- spec_ex_file(file)
  load_spec_proj(file)
}

##' @rdname load_spec_ex
##' @export
spec_ex_file <- function(file = "spec.yml") {
  system.file("spec", file, package = "yspec")
}

##' @rdname load_spec_ex
##' @export
spec_ex_spec <- function(file = "spec.yml") {
  spec_ex_file("spec.yml")
}

##' @rdname load_spec_ex
##' @export
spec_ex_proj <- function(file = "spec.yml") {
  spec_ex_file("project.yml")
}

