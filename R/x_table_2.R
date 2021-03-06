x_table_2_details <- function(x) {
  codes <- NULL
  if(.has("values", x)) {
    codes <- pack_codes(x)  
  }
  range <- NULL
  if(.has("range", x)) {
    range <- paste0("valid range: [", x$range[1], " to ",  x$range[2], "]")  
  }
  source <- NULL
  if(.has("source", x)) {
    source <- paste0("source: ", x$source)
  }
  details <- c(x$type,codes,range,source,x$comment) 
  paste0(details,collapse="; ")
}

x_table_2_table_row <- function(x,details_fun) {
  unit <- NULL
  if(.has("unit",x)) {
    unit <- glue::glue(" ({unit})", .envir = x)
  }
  short <- paste0(label.ycol(x,default = "short"),unit)
  ans <- tibble( 
    col = x$col, 
    label = short, 
    details = details_fun(x)
  )
  ans
}

x_table_2_df <- function(spec, 
                         row_fun = x_table_2_table_row, 
                         details_fun = x_table_2_details) {
  tab <- map_df(spec,row_fun,details_fun = details_fun)
  names(tab) <- c("Name", "Label", "Details") 
  tab
}

x_table_2 <- function(spec,
                      row_fun = x_table_2_table_row, 
                      details_fun = x_table_2_details) {
  
  tab <- x_table_2_df(spec, row_fun, details_fun)
  hlines <- which(tab[,1] != "")-1
  
  xt <- xtable(
    tab, 
    align = c("p{0cm}",
              "|p{0.8in}" , 
              ">{\\raggedright\\arraybackslash}p{2.39in}",
              ">{\\raggedright\\arraybackslash}p{2.78in}|")
  )
  add.to.row <- list(pos = list(0), command = NULL)
  command__ <- paste0(
    "\\hline\n\\endhead\n",
    "\\hline\n",
    "\\multicolumn{3}{l}",
    "{\\footnotesize Continued on next page}\n",
    "\\endfoot\n",
    "\\endlastfoot\n"
  )
  add.to.row$command <- command__
  pxt <- xtable::print.xtable(
    xt, 
    tabular.environment = "longtable", 
    latex.environments = NULL,
    include.rownames = FALSE,
    hline.after = c(-1,hlines),
    comment = FALSE,
    floating = FALSE,
    print.results = FALSE, 
    add.to.row = add.to.row,
    table.placement = "H",
    sanitize.text.function = getOption("ys.sanitize", ys_sanitize)
  )
  pxt <- glue::glue(pxt, .envir=get_meta(spec)$glue, .open = "<<", .close = ">>")
  return(pxt)  
}
