
library(yspec)
library(testthat)
library(purrr)
.test_load <- yspec:::.test_load
.test_spec <- yspec:::.test_spec

context("test-yspec")

spec <- load_spec_ex(file = "spec.yml")


test_that("yspec object", {
  expect_is(spec, "yspec")
  expect_true(is.list(spec))
  cl <- map_chr(spec, class)
  expect_true(all(cl=="ycol"))
})

test_that("yspec methods", {
  expect_is(as.list(spec), "list")
  expect_is(spec$WT, "ycol")
  expect_identical(spec$WT$col, "WT")
  expect_equal(spec$EGFR$range, c(10,300))
  expect_is(as.data.frame(spec), "data.frame")
  expect_is(head(spec), "data.frame")
})

test_that("ycol accessors", {
  expect_identical("kg", yspec:::unit(spec$WT))
  expect_identical("10 to 300", yspec:::Range(spec$EGFR))
  expect_identical("character", yspec:::type(spec$HAIR))
  expect_identical("dependent variable", yspec:::long(spec$DV))
  expect_identical("Compartment", yspec:::short(spec$CMT))
  expect_identical("per NONMEM specifications", yspec:::comment(spec$EVID))
  expect_identical("C", yspec:::lookup(spec$C))
  expect_identical("<none>", yspec:::lookup(spec$DV))
})


test_that("spec object meta data", {
  meta <- get_meta(spec)
  expect_is(meta[["spec_file"]], "character")
  expect_is(meta[["spec_path"]], "character")
  expect_is(meta$primary_key, "character")
  expect_identical(meta[["spec_path"]], dirname(meta[["spec_file"]]))
  expect_identical(basename(meta[["spec_file"]]), "spec.yml")
})

test_that("testing input data", {
  expect_error(.test_load("foo"),
               regexp = "names not found")
  expect_error(.test_load("foo", 1, 2, 3),
               regexp = "names not found")
  expect_error(.test_load("foo", name = "Kyle"),
               regexp = "invalid column field")
})

test_that("testing input columns", {
  expect_error(.test_load("foo", values = c(1,2,3),
                          range = c(0,100)),
               regexp = "both values and range")
  expect_error(.test_load("foo", values = c(1,2,3),
                          decode = c("a","b")),
               regexp = "the length of values is not equal to the length of decode")

})

file <- system.file("spec", "test_lookup.yml", package = "yspec")

test_that("column data is merged when lookup is true", {
  raw <- yspec:::try_yaml(file)
  mspec <- load_spec_ex(file = "test_lookup.yml")
  look <- yspec:::ys_get_lookup(mspec)
  
  expect_null(raw$BMI$unit)
  expect_identical(mspec$BMI$unit, look$BMI$unit)
})

test_that("column data is not merged when lookup is false", {
  raw <- yspec:::try_yaml(file)
  mspec <- load_spec_ex(file = "test_lookup.yml")
  look <- yspec:::ys_get_lookup(mspec)

  expect_null(raw$ALB$unit)
  expect_identical(mspec$BMI$unit, look$BMI$unit)

})

