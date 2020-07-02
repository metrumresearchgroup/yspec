chunk_and_indent <- function(x,indent,width) {
  if(x=="") return("")
  x <- paste0("[",x,"]")
  pre0 <- paste0(rep(" ", indent),collapse = "")
  pre <- paste0(pre0, " ; ")
  chunked <- strwrap(x, width = width, prefix = pre,exdent=1)
  paste0("\n",paste0(chunked,collapse = "\n"))
}

#' Write columns and column information for NONMEM 
#' 
#' @param spec a yspec object
#' @param width number of
nm_input <- function(spec,width = 60) {
  col <- map_chr(spec, "col")
  chr <- map_chr(spec, "type") == "character"
  col2 <- paste0(col, ifelse(chr & col != "C", "=DROP", ""))
  mx <- max(nchar(col2))
  col2 <- formatC(col2, width = mx, flag = "-")
  has_values <- map_lgl(spec, ~!is.null(.x$values))
  has_decode <- map_lgl(spec, ~!is.null(.x$decode))
  details <- map_chr(spec, pack_codes)
  short <- ys_get_short(spec, .aslist=FALSE)
  df <- tibble(col = col, col2 = col2, spc = " ; ", short = short, details = details)
  df <- mutate(df, details = map_chr(details, chunk_and_indent, indent = mx, width = width))
  df <- mutate(df, rhs = trimws(paste0(short," ", details)))
  out <- paste0(df$col2, df$spc, df$rhs)
  out <- unlist(strsplit(out, "\n"))
  c("$INPUT",out)
}
