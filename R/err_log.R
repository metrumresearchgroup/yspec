# .has <- yspec:::.has
# e <- new.env()
# e[["lst"]] <- vector(mode = "list")
# e[["nxt"]] <- 1
# e[["stp"]] <- FALSE
# e[["file"]] <- "none"
# 
# add_row <- function(e,df) {
#   df[["file"]] <- e[["file"]]
#   e[["lst"]][[e[["nxt"]]]] <- df
#   e[["nxt"]] <- e[["nxt"]] + 1
# }
# 
# col_context <- function(name,note) {
#   name <- paste0("col: ", name)
#   tibble(context = name, note = note)
# }
# 
# chk_decode_values <- function(x,e) {
#   if(.has("values",x) && .has("range",x)) {
#     msg <- "column has both values and range"
#     df <- col_context(x[["name"]],msg)
#     add_row(e,df)
#   }
# }
# 
# chk_decode_length <- function(x,e) {
#   if(.has("decode",x)) {
#     if(length(x[["values"]]) != length(x[["decode"]])) {
#       msg <- "length of values not equal to length of decode"
#       df <- col_context(x[["name"]],msg)
#       add_row(e,df)
#     }
#   }
# }
# 
# x <- list(values = c(1,2,3), range = c(1,2), name = "WT", decode = "A")
# 
# chk_decode_values(x,e)
# chk_decode_length(x,e)
# 
# 
# 
# 
# 
# 
# 
# 
