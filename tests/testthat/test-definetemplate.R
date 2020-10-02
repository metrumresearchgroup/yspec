library(yspec)
library(testthat)

context("test-definetemplate")

test_that("mrgtemplate is deprecated", {
  expect_warning(mrgtemplate())
})

test_that("definetemplate replaces mrgtemplate", {
  loc <- definetemplate()
  expect_true(dir.exists(loc))
  files <- c("_output.yml", "header.tex", "definetemplate.tex")
  exist <- file.exists(file.path(tempdir(), "definetemplate", files))
  expect_true(all(exist))
})
