library(yspec)
library(testthat)

context("test-definetemplate")

test_that("mrgtemplate is deprecated", {
  expect_warning(mrgtemplate())
})

test_that("definetemplate replaces mrgtemplate", {
  loc <- normalizePath(definetemplate())
  expect_true(dir.exists(loc))
  files <- c("_output.yml", "header.tex")
  where_to_check <- normalizePath(tempdir())
  exist <- file.exists(file.path(where_to_check, "definetemplate", files))
  expect_true(all(exist))
})
