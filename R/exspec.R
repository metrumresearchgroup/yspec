##' Generate an example specification object
##'
##' @param file an example spec file name
##'
##' @export
##'
ex_spec <- function(file = "spec.yml") {
 spec <- system.file("spec", file, package="yspec")
 load_spec(spec)
}
