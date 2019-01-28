
library(yspec)

context("test-utils")

test_that("double quoting", {
  expect_identical('"a"', yspec:::db_quote("a"))
})

test_that("err_file", {
  expect_error(
    yspec:::err_file("foo.txt", "this is an error"), 
    regexp = "this is an error"
  )  
})

test_that("list operations", {
  a <- list(a = 1, b = 2, c = 3)
  b <- list(b = 4, d = 5)
  
  x <- yspec:::combine_list(a,b)
  expect_equal(x, list(a=1,b=4,c=3,d=5))
  
  x <- yspec:::update_list(a,b)
  expect_equal(x, list(a = 1, b = 4, c = 3))
  
  x <- yspec:::merge.list(a,b)
  expect_equal(x,yspec:::update_list(a,b))
})

test_that("make_null", {
  a <- list(list(x = 1, y = 2, z = 3))
  b <- yspec:::make_null(a, "y")
  expect_equal(b, list(list(x = 1, z = 3)))
})

test_that(".stop", {
  expect_error(yspec:::.stop("abcde"), regexp="abcde")
})

test_that("sanitizers", {
  x <- "a _ b & c % d ^"
  ans <- ys_sanitize(x)
  expect_identical(ans, "a \\_ b \\& c \\% d \\verb|^|")
  
  ans <- ys_dont_sanitize(x)
  expect_identical(ans,x)
  
  ans <- ys_mild_sanitize(x)
  expect_identical(ans, "a _ b \\& c \\% d ^")
})


test_that("yspec_select", {
  spec <- ys_help$spec()
  x <- yspec_select_discrete(spec)
  expect_identical(
    names(x), 
    c("C","SUBJ", "SEQ", "EVID", "CP", "MDV", "BLQ", "PHASE", "STUDY", "RF")
  )
  x <- yspec_select_chr(spec)
  expect_identical(names(x), c("C", "SUBJ", "RF"))
})

