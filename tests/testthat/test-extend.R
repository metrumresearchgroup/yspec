library(yspec)
library(testthat)

context("test-extend.R")

main <- '
SETUP__:
  extend_file: foo.yml
A: 
  short: letter a
B: 
  short: letter b
'
ext_good <- '
C: 
  short: letter c
D: 
  short: letter d
'
ext_bad <- '
C: 
  short: letter c
B: 
  short: letter d
'

file <- yspec:::temp_spec(main, "extend-1")

test_that("load spec with extension", {
  ext <- yspec:::temp_spec(ext_good, "foo.yml")
  spec <- ys_load(file, extend = TRUE)
  expect_is(spec, "yspec")
  expect_equal(names(spec), LETTERS[1:4])
  spec <- ys_load(file)
  expect_is(spec, "yspec")
  expect_equal(names(spec), LETTERS[1:2])
})

test_that("extend a yspec object", {
  extension_file <- system.file("spec", "nm-extension.yml",  package = "yspec")
  spec <- ys_help$spec()
  prev <- names(spec)
  spec <- ys_extend(spec, extension_file)
  expect_is(spec, "yspec")
  diff <- setdiff(names(spec), prev)
  ext <- ys_load(extension_file)
  expect_identical(diff, names(ext))
})

test_that("extension fails when extension file doesn't exist", {
  ext <- yspec:::temp_spec(ext_bad, "foo.yml")
  unlink(ext, recursive = TRUE)
  expect_error(
    ys_load(file, extend = TRUE), 
    regexp = "Extension file does not exist"
  )
})
