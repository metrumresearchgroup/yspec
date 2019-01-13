
valid_setup_keys <- c(
  "primary_key", "lookup_file", 
  "description", "sponsor", "projectnumber", 
  "data_path", "data_stem", "name", "spec_file", 
  "spec_path"
)


check_bad_keys <- function(x,valid,where) {
  invalid <- setdiff(names(x),valid)
  if(length(invalid)==0) {
    return(invisible(NULL))  
  }
  bad <- paste0(" - ",invalid,"\n")
  bad <- c("Found invalid keys in ", where,"\n", bad)
  warning(bad, immediate.=TRUE,call.=FALSE)
}


spec_validate_meta <- function(meta) {
  check_bad_keys(meta, valid_setup_keys, "SETUP__:")
  assert_that(exists("name",meta))
  assert_that(exists("spec_file", meta))
  assert_that(exists("spec_path", meta))
  assert_that(exists("data_stem", meta))
  assert_that(exists("data_path", meta))
  assert_that(!is.null(meta[["data_stem"]]))
  assert_that(nchar(meta[["data_stem"]]) > 0)
  return(invisible(NULL))
}







