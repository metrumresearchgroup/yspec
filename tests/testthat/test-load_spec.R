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

test_that("invalid type value", {
  expect_error(
    yspec::test_spec_list(list(A = list(type = "factor"))), 
    "'type' must be 'numeric', 'character' or 'integer' \\('factor'\\)"
  )
})

test_that("primary keys not in the data set", {
  expect_error(
    ld("bad_keys.yml"), 
    "invalid primary key."
  )
})

test_that("error if column name is greater than 8 characters", {
  expect_error(ld("long_column.yml"), "more than 8 characters long")
  options(ys.col.len = 100)
  expect_warning(
    x <- ys_load(ys_help$file()), 
    "The option `ys.col.len` has been deprecated; please use"
  )
  options(ys.col.len = NULL)
})

test_that("error if unit, type, or short are gt length 1 issue-45", {
  expect_error(yspec:::test_spec_test("issue-45.yml"), 
               regexp="should not be more than length 1") 
})

test_that("access label", {
  x <- yspec:::test_spec_test("issue-60.yml") 
  expect_equal(yspec:::label.ycol(x$A), "the label for column A")
  expect_equal(yspec:::label.ycol(x$B), "the label (long) for column B")
  expect_equal(yspec:::label.ycol(x$C), "the label (short) for column C")
  ans <- list(yspec:::label(x$A),yspec:::label(x$B),yspec:::label(x$C,"short"))
  lab <- yspec:::label(x)
  expect_identical(names(lab), c("A", "B", "C"))
  expect_identical(unname(lab), ans)
})

test_that("error if label greater than 40 characters", {
  expect_error(yspec:::test_spec_error("long_label.yml"),
               regexp = "should not be longer than 40 characters") 
})

test_that("collapse source, comment, long issue-46", {
  x <- yspec:::test_spec_test("issue-46.yml") 
  x <- as.list(x)
  expect_equal(x$FOO$comment, "first line second line third line")
  expect_equal(x$FOO$source, "line one line two line 3")
  expect_equal(x$FOO$long, "a b c")
})

test_that("error to pass non-character file name", {
  x <- ys_help$spec()
  expect_error(ys_load(x))
})

