##' Generate an example specification object
##'
##' @export
##'
ex_spec <- function() {
 spec <- system.file("spec", "spec.yml",package="yspec")
 load_spec(spec)
}
