##' Render a spec as a pandoc table
##'
##' @param x a spec object
##' @param data_file not used
##' @param ... passed to other functions
##'
##' @export
pander_table <- function(x, data_file = "data", ...) {

  assert_that(requireNamespace("pander"))

  ans <- pander_table_df(x, data_file, ...)

  pander::pandoc.table(
    ans,
    justify = c("left", "left", "left", "left"),
    split.tables = 80,
    split.cells = c(5,25,8,25),
    style = "multiline"
  )
}


pander_table_df <- function(x, data_file = "data", ...) {

  cols <- vector(mode="list", length(x))

  for(i in seq_along(x)) {
    cols[[i]] <- define_col_pander(x[[i]])
  }

  bind_rows(cols)

}


define_col_pander <- function(x) {

  col <- x$col
  unit <- NULL
  source <- NULL
  comment <- NULL
  decode <- NULL
  chna <- as.character(NA)

  long <- long(x, chna)
  comment <- comment(x, chna)
  unit <- unit(x, character(0))
  col_ <- paste0(col, " ", unit)
  ran <- Range(x,chna)
  type <- type(x,'.')

  fields <- c("short-name", "long-name","comment","range")
  values <- c(x$short, long,  comment, ran)


  ans <- data_frame(
    col = col_,
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
