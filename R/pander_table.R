##' Render a spec as a pandoc table
##'
##' @param x a spec object
##' @param ... passed to other functions
##'
##' @examples
##' spec <- load_spec_ex()
##'
##' pander_table_df(spec)
##'
##' pander_table(spec)
##'
##' @export
pander_table <- function(x, ...) {

  assert_that(requireNamespace("pander"))

  ans <- pander_table_df(x)

  pander::pandoc.table.return(
    ans,
    justify = c("left", "left", "left", "left"),
    split.tables = 80,
    split.cells = c("22%","15%","22%","41%"),
    style = "multiline"
  )

}

##' @rdname pander_table
##' @export
x_table <- function(x,...) {
  assert_that(requireNamespace("xtable"))
  ans <- pander_table_df(x)
  lengths <- c(0, 1, 1, 1, 3)
  align <- paste0("p{",lengths,"in}")
  #align[1] <- paste0("|", align[1], "|")
  align[2] <- paste0("|", align[2], "|")
  align[4] <- paste0("|", align[4])
  align[5] <- paste0(align[5], "|")
  hlines <- which(ans[,1] != "")-1
  xx <- xtable(ans, align = align)
  capture.output(
    print(xx, hline.after=c(-1,hlines),
          add.to.row = add.to.row, comment = FALSE,
          include.rownames = FALSE, table.placement = "H",
          tabular.environment = "longtable", floating = FALSE)
  )

}

##' @rdname pander_table
##' @export
pander_table_df <- function(x) {
  map_df(x, define_col_pander)
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
      val <- paste(val, " = ", x[["decode"]])
    }
    val <- paste0(val, collapse=", ")
  } else {
    val <- chna
  }

  fields <- c("short-name",
              "unit",
              "range",
              "values",
              "long-name",
              "source",
              "comment")
  values <- c(x$short,
               unit,
               ran, val,
               long,
               src,
               comment)
  col <- x$col

  ans <- data_frame(
    col = col,
    type = type,
    field = fields,
    value = values)

  ans <- mutate(ans,
                col = if_else(duplicated(col), "", col),
                type = if_else(duplicated(type), "", type),
  )

  ans <- filter(ans, !is.na(value))

  set_names(ans, c("Column", "Type", "Field", "Value"))

}
