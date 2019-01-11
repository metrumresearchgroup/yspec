
spec_validate_meta <- function(meta) {
  assert_that(exists("name",meta))
  assert_that(exists("data_stem", meta))
  assert_that(exists("data_path", meta))
  assert_that(!is.null(meta[["data_stem"]]))
  assert_that(nchar(meta[["data_stem"]]) > 0)
  return(invisible(NULL))
}
