#' Change namespace
#' 
#' @param x a `yspec` object
#' @param namespace namespace name (character) to switch to
#'  
#' @return the `yspec` object (`x`) is returned, possibly modified
#' @export
ys_namespace <- function(x, namespace = NULL) {
  assert_that(is_yspec(x))
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

#' Find and process namespaced input
#' 
#' @param col a list of column data
#' @param col_name the column name (character)
#' 
#' @keywords internal
#' @noRd
#' 
#' @details
#' - find names in the list of the form `<field>.<namespace>` (e.g. `unit.tex`
#'   for the field `unit` in the `tex` namespace
#' - nothing is done if `.` isn't found 
#' - if `.` is found, we split the name and create a `namespace` slot in the 
#'   list where we hold the `<field>` information in a list named `<namespace>`
#' - when a namespace entry is made, we also copy any unnamespaced entry for 
#'   `field` into a `base` namespace
#' - when the `field` is `decode`, we check to make sure that the length of 
#'   `decode` in the namespace is the same as the length of `decode` in the 
#'   `base`
#' 
#' @return 
#' `col` is returned with possibly an extra slot called `namespace` 
#' 
create_namespaces <- function(col, col_name) {
  if(!is.list(col)) return(col)
  if(.has("namespace", col)) return(col)
  # see check_spec_input_col where we also split on `.` for ns information
  has_ns <- str_detect(names(col), fixed("."))
  if(!any(has_ns)) return(col)
  ns_input <- col[which(has_ns)]
  col <- col[which(!has_ns)]
  ns_parsed <- str_split_fixed(names(ns_input), fixed("."), 2)
  namespace <- list()
  for(i in seq(nrow(ns_parsed))) {
    ns <- ns_parsed[i,2]
    field <- ns_parsed[i,1]
    if(!exists(ns, namespace)) {
      namespace[[ns]] <- list()  
    }
    namespace[[ns]][[field]] <- ns_input[[i]]
    namespace[["base"]][[field]] <- col[[field]]
    if(field == "decode") {
      validate_namespace_decode(col_name, namespace)
    }
  }
  if(is.list(col[["namespace"]])) {
    col[["namespace"]] <- combine_list(col[["namespace"]],namespace)
  } else {
    col[["namespace"]] <- namespace  
  }
  col
}

#' Validate namespaced decode information
#' 
#' @param col a list of column data 
#' @param namespace a list of namespaced column data
#' 
#' @details
#' Check to make sure that the number of `decode` values in a namespace entry
#' matches the number of `decode` values in the base namespace.
#' 
#' @return an error is generated if decode lenght is no appropriate; otherwise, 
#' returns `NULL` invisibly
#' 
#' @keywords internal
#' @noRd
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
      "decode length in the namespace input must conform to base input", 
      call. = FALSE
    )
  }
  invisible(NULL)
}

try_tex_namespace <- function(x) {
  if("tex" %in% pull_meta(x, "namespace")) {
    x <- ys_namespace(x, "tex")  
  }
  x
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
