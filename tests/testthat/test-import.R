library(yspec)
library(purrr)
library(testthat)

context("test-import")

.sp <- yspec:::test_spec_test

test_that("import - identical spec", {
  a <- .sp("import-issue-84.yml")
  b <- .sp("DEM104101F_PKPD.yml")
  expect_identical(a,b)
})

test_that("import - identical spec with additional column", {
  a <- .sp("import-issue-84.yml")
  b <- .sp("import-add-issue-84.yml")
  expect_identical(
    c(names(a),"ADD"),
    names(b)
  )
})

