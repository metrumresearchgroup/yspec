#' Change namespace
#' 
#' @param x a `yspec` object
#' @param namespace namespace name (character) to switch to
#' 
#' @export
ys_namespace <- function(x, namespace = NULL) {
  if(is.null(namespace)) {
    ns <- pull_meta(spec, "namespace")
    if(length(ns)==0) {
      message("no namespaces found")
      return(invisible(spec))
    }
    message("namespaces:")
    message(paste0(" - ", ns, "\n"))
    return(invisible(spec))
  }
  namespace <- cvec_cs(namespace)
  for(ns in namespace) {
    spec <- modify(spec, switch_namespace, ns = ns, scan_for = VALID_NS_NAMES)
  }
  spec
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

list_namespaces <- function(spec) {
  ns <- map(spec, "namespace")
  ns <- keep(ns, is.list)
  ns <- map(ns, names)
  ns <- sort(unique(flatten_chr(ns)))  
  ns
}

create_namespaces <- function(col) {
  if(.has("namespace", col)) return(col)
  has_ns <- str_detect(names(col), fixed("::"))
  if(!any(has_ns)) return(col)
  y <- col[which(has_ns)]
  x <- col[which(!has_ns)]
  ns_data <- str_split_fixed(names(y), fixed("::"), 2)
  namespace <- list()
  for(i in seq(nrow(ns_data))) {
    if(!exists(ns_data[i,1], namespace)) {
      namespace[[ns_data[i,1]]] <- list()  
    }
    namespace[[ns_data[i,1]]][[ns_data[[i,2]]]] <- y[[i]]  
  }
  x$namespace <- namespace
  x
}

try_tex_namespace <- function(x) {
  if("tex" %in% pull_meta(x, "namespace")) {
    x <- ys_namespace(x, "tex")  
  }
  return(x)
}

