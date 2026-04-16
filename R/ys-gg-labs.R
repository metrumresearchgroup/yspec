#' Automatically label ggplot axes from a yspec object or other source
#'
#' @param spec a yspec object; axis data is generated through a call to 
#' [ys_get_short_unit()].
#' @param labs a named list of axis title data; names correspond to columns 
#' in the data used to make the plot; overrides `spec`.
#' @param x passed to [ggplot2::labs()] if character.
#' @param y passed to [ggplot2::labs()] if character.
#' @param fill passed to [ggplot2::labs()] if character.
#' @param col passed to [ggplot2::labs()] if character.
#' @param lty passed to [ggplot2::labs()] if character. 
#' @param shape passed to [ggplot2::labs()] if character.
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#' @export
ys_gg_labs <- function(spec = NULL, 
                       labs = list(), 
                       x = NULL, y = NULL,
                       fill = NULL, col = NULL,
                       lty = NULL, shape = NULL, ...) {
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
      col = col, 
      lty = lty,
      shape = shape,
      extra = list(...)
    ),
    class = "ys_gg_labs"
  )
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

#' @exportS3Method ggplot2::ggplot_add
ggplot_add.ys_gg_labs <- function(object, p, object_name) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  
  # Extract aesthetic mappings from the plot
  mapping <- p$mapping
  
  # Helper: resolve a variable name from a quosure
  aes_name <- function(q) {
    if (is.null(q)) return(NULL)
    rlang::as_label(q)
  }
  
  x_var <- aes_name(mapping$x)
  y_var <- aes_name(mapping$y)
  f_var <- aes_name(mapping$fill)
  c_var <- aes_name(mapping$colour)
  l_var <- aes_name(mapping$linetype)
  s_var <- aes_name(mapping$shape)
  
  # Resolve display labels via lookup table, with fallback
  resolve_label <- function(var, object, what) {
    if(is.character(object[[what]])) {
      return(object[[what]])  
    }
    envir <- object$envir
    if (is.null(var)) return(NULL)
    var <- strip_factor_call(var)
    if (!is.null(envir) && !is.null(envir[[var]])) {
      envir[[var]]
    } else {
      var
    }
  }
  
  args <- list()

  args$x <- resolve_label(x_var, object, "x")
  args$y <- resolve_label(y_var, object, "y")

  if(is.character(f_var)) {
    args$fill <- resolve_label(f_var, object, "fill")
  }

  if(is.character(c_var)) {
    args$colour <- resolve_label(c_var, object, "col")
  }
  
  if(is.character(l_var)) {
    args$lty <- resolve_label(l_var, object, "lty")
  }
  
  if(is.character(s_var)) {
    args$shape <- resolve_label(s_var, object, "shape")
  }

  lab_args <- c(args, object$extra)
  
  p + do.call(ggplot2::labs, lab_args)
}
