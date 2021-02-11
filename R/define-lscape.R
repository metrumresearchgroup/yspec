#' Create a data frame for landscape define document
#' 
#' @param spec a `yspec` object
#' @param title_case if `TRUE` the `Label` column is passed to 
#' [tools::toTitleCase()]
#' @param pmtable set this to `TRUE` to return a list object with class 
#' `pmtable` which can directly be turned into a latex table
#' 
#' @return A tibble with columns:
#' - `Variable`
#' - `Label`
#' - `Unit`
#' - `Type`
#' - `Decode/Range/Comment`
#' 
#' @export
lscape_table <- function(spec, title_case = FALSE, pmtable = FALSE) {
  spec <- try_tex_namespace(spec)
  label <- map_chr(spec, ys_get_label)
  if(isTRUE(title_case)) {
    label <- tools::toTitleCase(label)  
  }
  unit <- map_chr(spec, ys_get_unit)
  unit <- map_chr(
    unit, 
    glue::glue, 
    .envir = get_meta(spec)$glue, 
    .open = "<<", 
    .close = ">>"
  )
  type <- map_chr(spec, "type")
  col <- map_chr(spec, "col")
  comment <- map_chr(spec, "comment", .default = "")
  src <- map_chr(spec, "source", .default = "")
  values <- map_chr(spec, pack_codes)
  details <- map_chr(spec, lscape_details)
  details <- map_chr(
    details, 
    glue::glue, 
    .envir = get_meta(spec)$glue, 
    .open = "<<", 
    .close = ">>"
  ) 
  data <- tibble(
    Variable = col, 
    Label = label, 
    Unit = unit, 
    Type = type, 
    `Decode/Range/Comment` = details
  )
  if(!isTRUE(pmtable)) return(data)
  align <- pmtables::cols_align(
    Variable = pmtables::col_ragged(1.75), 
    Label = pmtables::col_ragged(5.5), 
    Unit = pmtables::col_ragged(2), 
    Type = pmtables::col_ragged(1.5),
    `Decode/Range/Comment` = pmtables::col_ragged(10)
  )
  nl <- seq(length(spec))
  ans <- list(data = data, align = align, hline_at = nl, cols_bold = TRUE)
  structure(ans, class = c("pmtable", class(ans)))
}
lscape_details <- function(x) {
  codes <- NULL
  if(.has("values", x)) {
    codes <- pack_codes(x)  
  }
  range <- NULL
  if(.has("range", x)) {
    u <- unit(x, default = "")
    range <- paste0("valid range: ", x$range[1], " to ",  x$range[2], " ", u)  
  }
  source <- NULL
  if(.has("source", x)) {
    source <- paste0("source: ", x$source)
  }
  details <- c(codes,range,source,x$comment) 
  paste0(details,collapse="; ")
}

#' @export
lscape <- function(spec) {
  ans <- pmtables::stable_long(lscape_table(spec, pmtable = TRUE))  
  ans <- pmtables::st_wrap(ans)
  ans
}
