

##' Format specification output with xtable
##' 
##' Requires the xtable package to be installed.  [pander_table_df] actually
##' formats the table. 
##' 
##' @param x a `yspec` object
##' @param ... not used
##' 
##' @examples
##' spec <- load_spec_ex()
##'
##' x_table(spec)
##'
##' @md
##' @export
x_table <- function(x,...) {
  assertthat::assert_that(requireNamespace("xtable"))
  tab <- pander_table_df(x,...)
  lengths <- c(0, 1, 0.65, 0.95, 2.4)
  align <- paste0(">{\\raggedright\\arraybackslash}p{",lengths,"in}")
  align[2] <- paste0("|", align[2], "|")
  align[4] <- paste0("|", align[4])
  align[5] <- paste0(align[5], "|")
  hlines <- which(tab[,1] != "")-1
  xtab <- xtable(tab, align = align)
  ans <- capture.output(
    print(
      xtab, hline.after=c(-1,hlines),
      add.to.row = add.to.row, comment = FALSE,
      include.rownames = FALSE, table.placement = "H",
      tabular.environment = "longtable", floating = FALSE,
      sanitize.text.function = getOption("ys.sanitize", ys_sanitize)
    )
  )
  glu <- get_meta(x)[["glue"]]
  if(is.list(glu)) {
    ans <- sapply(ans, glue, .envir = glu, .open = .glopen, .close = .glclose)
  }
  ans
}

#' @rdname x_table
#' @export
x_table_long <- function(x,...) {
  assertthat::assert_that(requireNamespace("xtable"))
  tab <- pander_table_df(x, long=TRUE, ...)
  lengths <- c(0, 1.65, 0.95, 2.4)
  align <- paste0(">{\\raggedright\\arraybackslash}p{",lengths,"in}")
  align[2] <- paste0("|", align[2], "|")
  align[4] <- paste0(align[4], "|")
  hlines <- which(tab[,1] != "")-1
  xtab <- xtable(tab, align = align)
  ans <- capture.output(
    print(
      xtab, hline.after=c(-1,hlines),
      add.to.row = add.to.row_long, comment = FALSE,
      include.rownames = FALSE, table.placement = "H",
      tabular.environment = "longtable", floating = FALSE,
      sanitize.text.function = getOption("ys.sanitize", ys_sanitize)
    )
  )
  glu <- get_meta(x)[["glue"]]
  if(is.list(glu)) {
    ans <- sapply(ans, glue, .envir = glu, .open = .glopen, .close = .glclose)
  }
  ans
}


# nocov start

##' DEPRECATED: Render a spec as a pandoc table
##' 
##' Requires the pander package to be installed.  [pander_table_df] actually
##' formats the table. 
##'
##' @param x a spec object
##' @param ... passed to other functions
##'
##' @examples
##' spec <- load_spec_ex()
##' 
##' \dontrun{
##' pander_table_df(spec)
##'
##' pander_table(spec)
##' }
##' 
##' @md
##' @export
pander_table <- function(x, ...) {
  
  stop("pander_table is deprecated")
  
  assertthat::assert_that(requireNamespace("pander"))
  
  ans <- pander_table_df(x)
  
  ans <- pander::pandoc.table.return(
    ans,
    justify = c("left", "left", "left", "left"),
    split.tables = 80,
    split.cells = c("22%","15%","22%","41%"),
    style = "multiline"
  )
  glu <- get_meta(x)[["glue"]]
  
  if(is.list(glu)) {
    ans <- sapply(ans, glue, .envir = glu, .open = .glopen, .close = .glclose)
  }
  
  ans
}

##' @param long if \code{TRUE}, then the table is made in a longer format
##' @rdname pander_table
##' @export
pander_table_df <- function(x, long=FALSE, ...) {
  if(long) {
    ans <- map_df(x, define_col_pander_long)    
  } else {
    ans <- map_df(x, define_col_pander)
  }
  return(ans)
}

define_col_pander <- function(x) {
  
  unit <- NULL
  source <- NULL
  comment <- NULL
  decode <- NULL
  chna <- as.character(NA)
  
  long <- long.ycol(x, chna)
  comment <- comment.ycol(x, chna)
  unit <- unit.ycol(x, chna)
  ran <- Range.ycol(x,chna)
  type <- type.ycol(x,'.')
  src <- Source.ycol(x,chna)
  
  if(.has("values", x)) {
    val <- x[["values"]]
    if(.has("decode",x)) {
      same <- which(val == x[["decode"]])
      val <- paste(val, " = ", x[["decode"]])
      val[same] <- x[["decode"]][same]
    }
    val <- paste0(val, collapse=", ")
  } else {
    val <- chna
  }
  
  fields <- c(
    "short-name",
    "unit",
    "range",
    "values",
    "long-name",
    "source",
    "comment"
  )
  values <- c(
    x$short,
    unit,
    ran, val,
    long,
    src,
    comment
  )
  col <- x$col
  
  ans <- tibble(
    col = col,
    type = type,
    field = fields,
    value = values
  )
  
  ans <- mutate(
    ans,
    col = if_else(duplicated(col), "", col),
    type = if_else(duplicated(type), "", type),
  )
  
  ans <- filter(ans, !is.na(value))
  
  set_names(ans, c("Column", "Type", "Field", "Value"))
  
}


define_col_pander_long <- function(x) {
  
  unit <- NULL
  source <- NULL
  comment <- NULL
  decode <- NULL
  chna <- as.character(NA)
  
  long <- long.ycol(x, chna)
  comment <- comment.ycol(x, chna)
  unit <- unit.ycol(x, chna)
  ran <- Range.ycol(x,chna)
  type <- type.ycol(x,'.')
  src <- Source.ycol(x,chna)
  
  if(.has("values", x)) {
    val <- x[["values"]]
    if(.has("decode",x)) {
      same <- which(val == x[["decode"]])
      val <- paste(val, " = ", x[["decode"]])
      val[same] <- x[["decode"]][same]
    }
    val <- paste0(val, collapse=", ")
  } else {
    val <- chna
  }
  
  fields <- c(
    "short-name",
    "type",
    "unit",
    "range",
    "values",
    "long-name",
    "source",
    "comment"
  )
  values <- c(
    x$short,
    type,
    unit,
    ran, 
    val,
    long,
    src,
    comment
  )
  col <- x$col
  
  ans <- tibble(
    col = col,
    field = fields,
    value = values
  )
  
  ans <- mutate(
    ans,
    col = if_else(duplicated(col), "", col)
  )
  
  ans <- filter(ans, !is.na(value))
  
  set_names(ans, c("Column", "Field", "Value"))

}

# nocov end
