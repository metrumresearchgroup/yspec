library(yspec)
library(testthat)

context("test-load_spec")

test_that("error is generated if spec file doesn't exist", {
  expect_error(load_spec("hey.yml"))
})

test_that("error if yaml code is bad", {
  expect_error(load_spec_ex("error.yml"),
               regexp = "failed to parse error.yml")
  expect_error(load_spec_ex("error.yml"),
               regexp = "Duplicate map key")
})

test_that("error if null values", {
  l <- list(NAME  = list(values = list(A=NULL, B = NULL)))
  expect_error(test_spec_list(l),label="values field includes NULLs")
})

test_that("decodes, named list", {
  l <- list(NAME  = list(values = list(A=1,B=2), type = "character"))
  x <- test_spec_list(l)
 expect_is(x, "yspec")
 expect_equal(x$NAME$decode, c("A", "B"))
 expect_equal(x$NAME$values, c(A=1,B=2))
 expect_equal(x$NAME$type,"character")
})

ld <- yspec:::test_spec_error

test_that("more decodes than values", {
  expect_error(
    ld("bad_decode.yml"),
    "length of values is not equal to"
    )
})

test_that("duplicate map key", {
  expect_error(
    ld("dup_map_key.yml"), 
    "Duplicate map key: 'WT'"
  )
})

test_that("column with no data", {
  expect_error(
    ld("no_data.yml"), 
    "ALB\n *- no spec data was found"
  )
})

test_that("primary keys not in the data set", {
  expect_error(
    ld("bad_keys.yml"), 
    "Invalid primary key."
  )
})

