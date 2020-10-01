#' Change namespace
#' 
#' @export
ys_namespace <- function(spec, namespace = NULL) {
  if(is.null(namespace)) {
    ns <- list_namespaces(spec)
    if(length(ns)==0) {
      message("no namespaces found")
      return(invisible(spec))
    }
    message("namespaces:")
    message(paste0(" - ", ns, "\n"))
    return(invisible(spec))
  }
  scan_for <- c("unit", "short", "label", "long", "decode")
  modify(spec, switch_namespace, namespace, scan_for = scan_for)
}

switch_namespace <- function(col, namespace, scan_for) {
  if(is.null(col$namespace[[namespace]])) {
    return(col)  
  }
  to_cp <- intersect(names(col), names(col[["namespace"]][[namespace]]))
  for(this_col in to_cp) {
    col[[this_col]] <- col[["namespace"]][[namespace]][[this_col]]
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
  if("namespace" %in% names(col)) return(col)
  z <- list()
  has_ns <- str_detect(names(col), fixed("::"))
  y <- col[which(has_ns)]
  x <- col[which(!has_ns)]
  if(length(y)==0) {
    x$namespace <- z
    return(x)
  }
  ns <- str_split_fixed(names(y), fixed("::"), 2)
  for(i in seq(nrow(ns))) {
    if(!exists(ns[i,1], z)) {
      z[[ns[i,1]]] <- list()  
    }
    z[[ns[i,1]]][[ns[[i,2]]]] <- y[[i]]  
  }
  x$namespace <- z
  x
}