
check_bad_keys <- function(x,valid,where) {
  invalid <- setdiff(names(x),valid)
  if(length(invalid)==0) {
    return(invisible(NULL))  
  }
  bad <- paste0(" - ",invalid,"\n")
  bad <- c("Unrecognized keys in ", where,"\n", bad)
  warning(bad, immediate.=TRUE, call.=FALSE)
}

spec_validate_meta <- function(meta) {
  check_bad_keys(meta, VALID_SETUP_NAMES, "SETUP__:")
  assert_that(exists("name",meta))
  assert_that(exists("spec_file", meta))
  assert_that(exists("spec_path", meta))
  assert_that(exists("data_stem", meta))
  assert_that(exists("data_path", meta))
  assert_that(!is.null(meta[["data_stem"]]))
  assert_that(nchar(meta[["data_stem"]]) > 0)
  if(exists("import", meta)) {
    assert_that(file.exists(meta[["import"]]))  
  }
  return(invisible(NULL))
}
