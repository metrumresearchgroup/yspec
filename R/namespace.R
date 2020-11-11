#' Change namespace
#' 
#' @param x a `yspec` object
#' @param namespace namespace name (character) to switch to
#' 
#' @export
ys_namespace <- function(x, namespace = NULL) {
  if(is.null(namespace)) {
    ns <- pull_meta(x, "namespace")
    if(length(ns)==0) {
      message("no namespaces found")
      return(invisible(x))
    }
    message("namespaces:")
    message(paste0(" - ", ns, "\n"))
    return(invisible(x))
  }
  available_namespaces <- pull_meta(x, "namespace")
  namespace <- cvec_cs(namespace)
  for(ns in namespace) {
    assert_that(
      ns %in% available_namespaces, 
      msg = glue("`{ns}` is not a namespace in this specification object")
    )
    x <- modify(x, switch_namespace, ns = ns, scan_for = VALID_NS_NAMES)
  }
  x <- set_namespace(x, namespace)    
  x
}

switch_namespace <- function(col, ns, scan_for) {
  if(is.null(col[["namespace"]][[ns]])) {
    return(col)  
  }
  to_cp <- intersect(names(col[["namespace"]][[ns]]),scan_for)
  for(this_col in to_cp) {
    col[[this_col]] <- col[["namespace"]][[ns]][[this_col]]
  }
  col
}

list_namespaces <- function(x) {
  ns <- map(x, "namespace")
  ns <- keep(ns, is.list)
  ns <- map(ns, names)
  ns <- sort(unique(flatten_chr(ns)))  
  ns
}

create_namespaces <- function(col, col_name) {
  if(!is.list(col)) return(col)
  if(.has("namespace", col)) return(col)
  has_ns <- str_detect(names(col), fixed("."))
  if(!any(has_ns)) return(col)
  y <- col[which(has_ns)]
  x <- col[which(!has_ns)]
  ns_data <- str_split_fixed(names(y), fixed("."), 2)
  namespace <- list()
  for(i in seq(nrow(ns_data))) {
    ns <- ns_data[i,2]
    field <- ns_data[i,1]
    if(!exists(ns, namespace)) {
      namespace[[ns]] <- list()  
    }
    namespace[[ns]][[field]] <- y[[i]]
    namespace[["base"]][[field]] <- x[[field]]
    if(field == "decode") {
      validate_namespace_decode(col_name, namespace)
    }
  }
  x$namespace <- namespace
  x
}

validate_namespace_decode <- function(col, namespace) {
  expected <- length(namespace[["base"]][["decode"]])
  ns <- names(namespace)
  for(i in seq_along(ns)) {
    if(length(namespace[[i]][["decode"]]) == expected) next
    found <- length(namespace[[i]][["decode"]])
    message("decode is not the correct length:")
    message(" - column: ", col)
    message(" - input:  ", paste0("decode.", ns[[i]]))
    message(" - expect: ", expected)
    message(" - actual: ", found)
    stop(
      "decode length in the namespaced input must conform to base input", 
      call. = FALSE
    )
  }
}

try_tex_namespace <- function(x) {
  if("tex" %in% pull_meta(x, "namespace")) {
    x <- ys_namespace(x, "tex")  
  }
  return(x)
}

current_namespace <- function(x) {
  attr(x, "namespace")
}

set_namespace <- function(x, ns) { 
  assert_that(is.character(ns))
  if(identical(ns, "base")) {
    return(structure(x, namespace = ns))  
  }
  structure(x, namespace = c(current_namespace(x), ns))
}
