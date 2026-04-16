#' Automatically label ggplot axes from a yspec object or other source
#'
#' @param spec a yspec object; axis data is generated through a call to 
#' [ys_get_short_unit()].
#' @param labs a named list of axis title data; names correspond to columns 
#' in the data used to make the plot; overrides `spec`.
#' @param xcol the name of the x-axis data column; overrides `labs` and `spec`.
#' @param ycol the name of the y-axis data column; overrides `labs` and `spec`.
#' @param xlab the complete title to use for the x-axis; overrides `xcol` and 
#' `spec`.
#' @param ylab the complete title to use for the y-axis; overrides `ycol` and 
#' `spec`. 
#' @param ... additional arguments passed to [ggplot2::labs()].
#'
#' @return A gg object that can be added to a ggplot with `+`.
#' @export
ys_gg_labs <- function(spec = NULL, 
                       labs = list(), 
                       x = NULL, y = NULL,
                       fill = NULL, col = NULL,
                       lty = NULL, ...) {
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
      extra = list(...)
    ),
    class = "ys_gg_labs"
  )
}

strip_factor <- function(var) {
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
  
  # Resolve display labels via lookup table, with fallback
  resolve_label <- function(var, object, what) {
    if(is.character(object[[what]])) {
      return(object[[what]])  
    }
    envir <- object$envir
    if (is.null(var)) return(NULL)
    var <- strip_factor(var)
    if (!is.null(envir) && !is.null(envir[[var]])) {
      envir[[var]]
    } else {
      var
    }
  }
  
  x_lab <- resolve_label(x_var, object, "x")
  y_lab <- resolve_label(y_var, object, "y")

  lab_args <- c(
    list(x = x_lab, y = y_lab),
    object$extra
  )
  
  if(is.character(f_var)) {
    f_lab <- resolve_label(f_var, object, "fill")
    lab_args$fill <- f_lab
  }

  if(is.character(c_var)) {
    c_lab <- resolve_label(c_var, object, "col")
    lab_args$colour <- c_lab
  }
  
  if(is.character(l_var)) {
    l_lab <- resolve_label(l_var, object, "lty")
    lab_args$lty <- l_lab
  }
  
  p + do.call(ggplot2::labs, lab_args)
}
