chunk_and_indent <- function(x,indent,width) {
  if(x=="") return("")
  x <- paste0("[",x,"]")
  pre0 <- paste0(rep(" ", indent),collapse = "")
  pre <- paste0(pre0, " ; ")
  chunked <- strwrap(x, width = width, prefix = pre,exdent=1)
  paste0("\n",paste0(chunked,collapse = "\n"))
}

#' Write columns and column information for NONMEM $INPUT block
#' 
#' Column names are written to the console one-per-line on the 
#' left hand side of a NONMEM comment character (`;`) and 
#' a description of the column is written on the right hand 
#' side.  The description includes the short name and values 
#' with decodes for non-character columns. 
#' 
#' @param spec a yspec object
#' @param width number of characters
#' @param cat if `TRUE`, the text is sent to the console with [cat()]
#' 
#' @return 
#' A character vector of text forming the `$INPUT` block (including 
#' the `$INPUT` part).
#' 
#' @examples
#' spec <- ys_help$spec()
#' nm_input(spec)
#' @md
#' @export
nm_input <- function(spec,width = 60,cat=TRUE) {
  col <- map_chr(spec, "col")
  chr <- map_chr(spec, "type") == "character"
  col2 <- paste0(col, ifelse(chr & col != "C", "=DROP", ""))
  mx <- max(nchar(col2))
  col2 <- formatC(col2, width = mx, flag = "-")
  has_values <- map_lgl(spec, ~!is.null(.x$values))
  has_decode <- map_lgl(spec, ~!is.null(.x$decode))
  is_chr <- map_lgl(spec, ~.x$type=="character")
  details <- map_chr(spec, pack_codes)
  details[is_chr] <- ""
  short <- ys_get_short(spec, .aslist=FALSE)
  df <- tibble(
    col = col, 
    col2 = col2, 
    spc = " ; ", 
    short = short, 
    details = details
  )
  df <- mutate(
    df, 
    details = map_chr(details, chunk_and_indent, indent = mx, width = width)
  )
  df <- mutate(df, rhs = trimws(paste0(short," ", details)))
  out <- paste0(df$col2, df$spc, df$rhs)
  out <- unlist(strsplit(out, "\n"))
  ans <- c("$INPUT",out)
  if(isTRUE(cat)) cat(ans, sep = "\n")
  return(invisible(ans))
}
