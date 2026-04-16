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
                       xcol = NULL, ycol = NULL, 
                       xlab = NULL, ylab = NULL, ...) {
  envir <- list()
  if(is_yspec(spec)) {
    envir <- c(envir, ys_get_short_unit(spec))
  }
  envir <- c(labs, envir)
  envir <- envir[!duplicated(names(envir))]
  structure(
    list(
      envir = envir, 
      xcol = xcol, 
      ycol = ycol, 
      xlab = xlab,
      ylab = ylab, 
      extra = list(...)
    ),
    class = "ys_gg_labs"
  )
}

#' @export
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
  
  # Resolve display labels via lookup table, with fallback
  resolve_label <- function(var, envir, col = NULL) {
    if (is.null(var)) return(NULL)
    if(is.character(col)) {
      stopifnot("column not found" = col %in% names(envir))
      return(envir[[col]])  
    }
    if (!is.null(envir) && !is.null(envir[[var]])) {
      envir[[var]]
    } else {
      var
    }
  }
  
  if(is.character(object$xlab)) {
    x_lab <- object$xlab
  } else {
    x_lab <- resolve_label(x_var, object$envir, col = object$xcol)  
  }
  
  if(is.character(object$ylab)) {
    y_lab <- object$ylab
  } else {
    y_lab <- resolve_label(y_var, object$envir, col = object$ycol)  
  }

  lab_args <- c(
    list(x = x_lab, y = y_lab),
    object$extra
  )
  
  p + do.call(ggplot2::labs, lab_args)
}
