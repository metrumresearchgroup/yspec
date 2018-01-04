##' Generate an example specification object
##'
##' @param file an example spec file name
##'
##' @export
##'
load_spec_ex <- function(file = "spec.yml") {
 spec <- system.file("spec", file, package="yspec")
 load_spec(spec)
}
