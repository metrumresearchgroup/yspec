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

test_that("load spec with extension [YSP-TEST-0036]", {
  ext <- yspec:::temp_spec(ext_good, "foo.yml")
  spec <- ys_extend(ys_load(file))
  expect_is(spec, "yspec")
  expect_equal(names(spec), LETTERS[1:4])
  spec <- ys_load(file)
  expect_is(spec, "yspec")
  expect_equal(names(spec), LETTERS[1:2])
})

test_that("extend a yspec object [YSP-TEST-0037]", {
  extension_file <- system.file("spec", "nm-extension.yml",  package = "yspec")
  spec <- ys_help$spec()
  prev <- names(spec)
  spec <- ys_extend(spec, extension_file)
  expect_is(spec, "yspec")
  diff <- setdiff(names(spec), prev)
  ext <- ys_load(extension_file)
  expect_identical(diff, names(ext))
})

test_that("extension fails when extension file doesn't exist [YSP-TEST-0038]", {
  ext <- yspec:::temp_spec(ext_bad, "foo.yml")
  unlink(ext, recursive = TRUE)
  expect_error(
    ys_extend(ys_load(file)), 
    regexp = "Extension file does not exist"
  )
})

# See test-tidy.R for join
test_that("namespace is merged on extension", {
  spec1 <- yspec::test_spec_list(list(                                                                                     
    A = list(unit = "ng", unit.pk = "ng/mL", unit.table = "\\frac{ng}{mL}"),                                                           
    B = list(short = "Dose amount")                                                                   
  ))
  expect_equal(pull_meta(spec1, "namespace"), c("base", "pk", "table"))
  ext <- yaml::as.yaml(list(                                                                                     
    AUC = list(short = "AUC", short.ss = "AUC,ss"),                                                           
    CMIN = list(short = "Cmin")                                                                   
  ))
  temp <- tempfile()
  writeLines(text = ext, temp)
  spec2 <- ys_extend(spec1, file = temp, silent = TRUE)
  expect_equal(pull_meta(spec2, "namespace"), c("base", "pk", "table", "ss"))
})
