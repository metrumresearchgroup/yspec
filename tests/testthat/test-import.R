library(yspec)
library(testthat)

context("test-import")

.sp <- yspec:::test_spec_test

test_that("import - identical spec", {
  a <- .sp("import-issue-84.yml")
  b <- .sp("DEM104101F_PKPD.yml")
  expect_identical(as.list(a),as.list(b))
})

test_that("import - meta data is not imported", {
  a <- .sp("import-issue-84.yml")
  b <- .sp("DEM104101F_PKPD.yml")
  expect_identical(pull_meta(a,"name"), "import-issue-84")
  expect_identical(pull_meta(a,"data_stem"), "import-issue-84")
  expect_identical(basename(pull_meta(a,"spec_file")), "import-issue-84.yml")
})

test_that("import - identical spec with additional column", {
  a <- .sp("import-issue-84.yml")
  b <- .sp("import-add-issue-84.yml")
  expect_identical(
    c(names(a),"ADD"),
    names(b)
  )
})


