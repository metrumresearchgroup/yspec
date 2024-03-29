library(yspec)
library(testthat)

context("test-definetemplate")

test_that("mrgtemplate is deprecated [YSP-TEST-0032]", {
  expect_warning(mrgtemplate())
})

test_that("definetemplate replaces mrgtemplate [YSP-TEST-0033]", {
  loc <- normalizePath(definetemplate())
  expect_true(dir.exists(loc))
  files <- c("_output.yml", "header.tex")
  where_to_check <- normalizePath(tempdir())
  exist <- file.exists(file.path(where_to_check, "definetemplate", files))
  expect_true(all(exist))
})
