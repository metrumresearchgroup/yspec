chunk_and_indent <- function(x,indent,width) {
  if(x=="") return("")
  x <- paste0("[",x,"]")
  pre0 <- paste0(rep(" ", indent),collapse = "")
  pre <- paste0(pre0, " ; ")
  chunked <- strwrap(x, width = width, prefix = pre,exdent=1)
  paste0("\n",paste0(chunked,collapse = "\n"))
}

make_input_names <- function(spec, drop, ...) {
  n <- names(spec)
  pos <- eval_rename(expr(c(...)), data = as.list(spec))
  pos <- pos[!names(pos) == n[pos]]
  type <- map_chr(spec, "type")
  drop <- c(drop, n[type=="character" & n != "C"]) 
  drop <- unique(drop)
  # Don't rename columns that are getting dropped
  pos <- pos[!n[pos] %in% drop]
  # Check that we don't have duplicate column names
  unique <- c(names(pos), n[-pos])
  # Ok to reuse a name if it is getting dropped
  unique <- unique[!unique %in% drop]
  if(anyDuplicated(unique)) {
    dup <- unique[duplicated(unique)]
    names(dup) <- "x"
    abort("Duplicated input names are not allowed.", body = dup)
  }
  n[pos] <- paste0(names(pos), "=", n[pos])
  dropn <- match(drop, n)
  if(length(drop)) {
    n[dropn] <- paste0(n[dropn], "=DROP")    
  }
  n
}

#' Create NONMEM $INPUT block code from yspec object
#' 
#' Extract column names from the yspec object and format for inclusion in 
#' a NONMEM control stream, potentially renaming or dropping columns. 
#' The default output is in wide format, including only the column names as 
#' well as any rename or drop information. The long format puts each column 
#' on a different line and includes the short name and optionally column 
#' decode information. See examples.
#' 
#' @param spec a yspec object.
#' @param .width passed to [base::strwrap()] to limit the output line length.
#' @param .cat if `TRUE`, the text is sent to the console with [cat()].
#' @param .drop a character vector or comma-separated string of columns to 
#' drop in the `$INPUT` listing; columns with character type are automatically 
#' dropped, so there is no need to list them here.
#' @param .long if `TRUE`, produce `$INPUT` in long, verbose format.
#' @param .decodes if `TRUE`, print `value` and `decode` information where 
#' available for discrete column data; this is only printed when `.long = TRUE`. 
#' @param ... unquoted column rename pairs with format 
#' `<new name> = <old name>`.
#' 
#' @details
#' Columns with character type are automatically dropped; there is no need 
#' to list these under the `.drop` argument.
#' 
#' @return 
#' A character vector of text forming the `$INPUT` block (including 
#' the `$INPUT` part).
#' 
#' @examples
#' spec <- ys_help$spec()
#' nm_input(spec)
#' nm_input(spec, DOSE = AMT, .drop = "ALT,BMI")
#' nm_input(spec, .long = TRUE)
#' nm_input(spec, .long = TRUE, .decodes = TRUE)
#' 
#' @md
#' @export
nm_input <- function(spec, .width = 65, .cat = TRUE, .long = FALSE,
                     .drop = NULL, .decodes = FALSE, ...) {
  
  .drop <- cvec_cs(.drop)
  if(!all(.drop %in% names(spec))) {
    baddrop <- setdiff(.drop, names(spec))
    baddrop <- paste0("Column `", baddrop, "` doesn't exist.")
    names(baddrop) <- rep("x", length(baddrop))
    abort(
      "Can't drop columns that don't exist.", 
      body = baddrop, 
      use_cli_format = TRUE
    )
  }
  if(isTRUE(.long)) {
    out <- nm_input_long(spec, .drop = .drop, .width = .width, 
                         .decodes = .decodes, ...)  
  } else {
    out <- nm_input_wide(spec, .drop = .drop, .width = .width, ...)  
  }
  ans <- c("$INPUT", out)
  if(isTRUE(.cat)) cat(ans, sep = "\n")
  return(invisible(ans))
}

nm_input_wide <- function(spec, .drop = NULL, .width = 65, ...) {
  ans <- make_input_names(spec, .drop, ...)
  ans <- paste0(ans, collapse = " ")
  ans <- strwrap(ans, width = .width)
  ans
}

nm_input_long <- function(spec, .drop, .width, .decodes, ...) {
  col <- make_input_names(spec, .drop, ...)
  mx <- max(nchar(col))
  col2 <- formatC(col, width = mx, flag = "-")
  details <- NULL
  if(isTRUE(.decodes)) {
    has_values <- map_lgl(spec, ~!is.null(.x$values))
    has_decode <- map_lgl(spec, ~!is.null(.x$decode))
    is_chr <- map_lgl(spec, ~.x$type=="character")
    details <- map_chr(spec, pack_codes)
    details[is_chr] <- ""
  } else {
    details <- rep("", length(spec))  
  }
  short <- ys_get_short(spec, .aslist = FALSE)
  df <- tibble(
    col = map_chr(spec, "col"), 
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
  out <- unlist(strsplit(out, "\n"), use.names=FALSE)
  out
}
