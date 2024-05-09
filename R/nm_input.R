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
nm_input <- function(spec, .width = 65, .cat = TRUE, .long = FALSE,
                     .drop = NULL, ...) {
  if(isTRUE(.long)) {
    out <- nm_input_long(spec, .width = .width)  
  } else {
    out <- nm_input_wide(spec, .width = .width, .drop = .drop, ...)  
  }
  ans <- c("$INPUT", out)
  if(isTRUE(.cat)) cat(ans, sep = "\n")
  return(invisible(ans))
}

nm_input_wide <- function(spec, .drop = NULL, .width = 65, ...) {
  n <- names(spec)
  pos <- eval_rename(expr(c(...)), data = as.list(spec))
  pos <- pos[!names(pos) == n[pos]]
  drop <- cvec_cs(.drop)
  if(!all(drop %in% names(spec))) {
    baddrop <- setdiff(drop, names(spec))
    baddrop <- paste0("Column `", baddrop, "` doesn't exist.")
    names(baddrop) <- rep("x", length(baddrop))
    abort(
      "Can't drop columns that don't exist.", 
      body = baddrop, 
      use_cli_format = TRUE
    )
  }
  type <- map_chr(spec, "type")
  drop <- c(drop, n[type=="character" & n != "C"]) 
  drop <- unique(drop)
  drop <- drop[drop %in% n]
  pos <- pos[!names(pos) %in% drop]
  n[pos] <- paste0(names(pos), "=", n[pos])
  dropn <- match(drop, n)
  if(length(drop)) {
    n[dropn] <- paste0(n[dropn], "=DROP")    
  }
  n <- paste0(n, collapse = " ")
  n <- strwrap(n, width = .width)
  n
}

nm_input_long <- function(spec, .width) {
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
  short <- ys_get_short(spec, .aslist = FALSE)
  df <- tibble(
    col = col, 
    col2 = col2, 
    spc = " ; ", 
    short = short, 
    details = details
  )
  df <- mutate(
    df, 
    details = map_chr(details, chunk_and_indent, indent = mx, width = .width)
  )
  df <- mutate(df, rhs = trimws(paste0(short," ", details)))
  out <- paste0(df$col2, df$spc, df$rhs)
  out <- unlist(strsplit(out, "\n"))
  out
}
