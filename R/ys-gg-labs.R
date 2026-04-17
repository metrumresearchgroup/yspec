#' Automatically label ggplot aesthetics from a yspec object or other source
#'
#' @param spec a yspec object; label data is generated through a call to
#' [ys_get_short_unit()].
#' @param labs a named list of label data; names correspond to columns
#' in the data used to make the plot; overrides `spec`.
#' @param x override label for the x aesthetic; if `NULL`, resolved from `spec`
#' or `labs`.
#' @param y override label for the y aesthetic; if `NULL`, resolved from `spec`
#' or `labs`.
#' @param fill override label for the fill aesthetic; if `NULL`, resolved from
#' `spec` or `labs`.
#' @param colour override label for the colour aesthetic; if `NULL`, resolved
#' from `spec` or `labs`; `color` and `col` are accepted as aliases.
#' @param linetype override label for the linetype aesthetic; if `NULL`,
#' resolved from `spec` or `labs`; `lty` is accepted as an alias.
#' @param shape override label for the shape aesthetic; if `NULL`, resolved from
#' `spec` or `labs`.
#' @param color alias for `colour`.
#' @param col alias for `colour`.
#' @param lty alias for `linetype`.
#' @param warn if `TRUE` (default), warn when the same aesthetic is mapped to
#' multiple variables that each have a spec entry but resolve to different
#' labels.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#' 
#' @examples
#' if(requireNamespace("ggplot2")) {
#'   library(ggplot2)
#'   
#'   spec <- ys_help$spec()
#' 
#'   spec <- update_short(spec, TIME = "Time")
#'   
#'   data <- ys_help$data()
#'   
#'   p <- ggplot(data, aes(TIME, DV)) + geom_point()
#'   
#'   p + ys_gg_labs(spec)
#' }
#' 
#' @md
#' @export
ys_gg_labs <- function(spec = NULL,
                       labs = list(),
                       x = NULL, y = NULL,
                       fill = NULL,
                       colour = NULL, color = NULL, col = NULL,
                       linetype = NULL, lty = NULL,
                       shape = NULL,
                       warn = TRUE, ...) {
  colour <- colour %||% color %||% col
  linetype <- linetype %||% lty
  envir <- list()
  if(is_yspec(spec)) {
    envir <- c(envir, ys_get_short_unit(spec))
  }
  envir <- c(labs, envir)
  envir <- envir[!duplicated(names(envir))]
  structure(
    list(
      envir = envir,
      x = x,
      y = y,
      fill = fill,
      colour = colour,
      linetype = linetype,
      shape = shape,
      warn = warn,
      extra = list(...)
    ),
    class = "ys_gg_labs"
  )
}

aes_name <- function(q) {
  if(is.null(q)) return(NULL)
  as_label(q)
}

strip_factor_call <- function(var) {
  fct <- grepl("factor", var, fixed  = TRUE)
  if(!fct) return(var)
  vars <- all.vars(str2lang(var), functions = TRUE)
  vars <- vars[vars != "factor"]
  if(length(vars)==1) {
    vars
  } else {
    var
  }
}

resolve_label <- function(var, envir) {
  if(is.null(var)) return(NULL)
  var <- strip_factor_call(var)
  if(!is.null(envir) && !is.null(envir[[var]])) envir[[var]] else var
}

resolve_aes_label <- function(aes, all_mappings, object) {
  if(is.character(object[[aes]])) return(object[[aes]])
  qs <- all_mappings[names(all_mappings) == aes]
  if(length(qs) == 0) return(NULL)
  vars <- vapply(qs, aes_name, character(1))
  labels <- vapply(vars, resolve_label, character(1), envir = object$envir)
  vars_stripped <- vapply(vars, strip_factor_call, character(1))
  in_envir <- vapply(vars_stripped, \(v) !is.null(object$envir[[v]]), logical(1))
  if(isTRUE(object$warn) && sum(in_envir) > 1 && length(unique(labels[in_envir])) > 1) {
    warning(
      paste0(
        "Aesthetic '", aes, "' is mapped to multiple variables (",
        paste(vars, collapse = ", "),
        ") that resolve to different labels; label for '", vars[1], "' will be used."
      ),
      call. = FALSE
    )
  }
  labels[1]
}

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ys_gg_labs <- function(object, p, object_name) {

  assert_that(requireNamespace("ggplot2"))

  layer_mappings <- do.call(c, lapply(unname(p$layers), \(l) l$mapping))
  all_mappings <- c(p$mapping, layer_mappings)

  args <- list()
  args$x <- resolve_aes_label("x", all_mappings, object)
  args$y <- resolve_aes_label("y", all_mappings, object)
  for(aes in c("fill", "colour", "linetype", "shape")) {
    label <- resolve_aes_label(aes, all_mappings, object)
    if(!is.null(label)) args[[aes]] <- label
  }

  p + do.call(ggplot2::labs, c(args, object$extra))
}
