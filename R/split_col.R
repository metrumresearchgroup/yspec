
unpack_split_col <- function(x) {
  .stop("not in service")
  # x <- unpack_about(x)
  # if(!is.character(x[["when"]])) x[["when"]] <- NA
  # x <- make_axis_label(x)
  # x <- make_col_label(x)
  # x
}

unpack_split <- function(x,col) {
  .stop("not in service")
  # for(n in names(x)) {
  #   x[[n]][["col"]] <- col
  #   x[[n]][["name"]] <- n
  #   x[[n]] <- unpack_split_col(x[[n]])
  # }
  # x
}
pack_split <- function(sp) {
  .stop("not in service")
  # if(!exists("split",sp)) return(character(0))
  # sp <- sp$split
  # short <- sapply(sp, `[[`, "short")
  # unit <- sapply(sp, `[[`, "unit")
  # unit[is.na(unit)] <- ""
  # unit[nchar(unit)>1] <- paste0("`",unit[nchar(unit)>1],"`")
  # when <- sapply(sp, `[[`, "when")
  # when[is.na(when)] <- ""
  # p <- paste0("- ", short," ", unit)
  # p[when !=""] <- paste0(p[when!=""], " when `", when[when!=""], "`")
  # p
}


