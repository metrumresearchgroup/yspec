
library(yspec)

context("test-utils")

test_that("double quoting [YSP-TEST-0114]", {
  expect_identical('"a"', yspec:::db_quote("a"))
})

test_that("err_file [YSP-TEST-0115]", {
  expect_error(
    yspec:::err_file("foo.txt", "this is an error"), 
    regexp = "this is an error"
  )  
})

test_that("list operations [YSP-TEST-0116]", {
  a <- list(a = 1, b = 2, c = 3)
  b <- list(b = 4, d = 5)
  
  x <- yspec:::combine_list(a,b)
  expect_equal(x, list(a=1,b=4,c=3,d=5))
  
  x <- yspec:::update_list(a,b)
  expect_equal(x, list(a = 1, b = 4, c = 3))
  
  x <- yspec:::merge.list(a,b)
  expect_equal(x,yspec:::update_list(a,b))
})

test_that("make_null [YSP-TEST-0117]", {
  a <- list(list(x = 1, y = 2, z = 3))
  b <- yspec:::make_null(a, "y")
  expect_equal(b, list(list(x = 1, z = 3)))
})

test_that(".stop [YSP-TEST-0118]", {
  expect_error(yspec:::.stop("abcde"), regexp="abcde")
})

test_that("sanitizers [YSP-TEST-0119]", {
  x <- "a _ b & c % d ^"
  ans <- ys_sanitize(x)
  expect_identical(ans, "a \\_ b \\& c \\% d \\verb|^|")
  
  ans <- ys_dont_sanitize(x)
  expect_identical(ans,x)
  
  ans <- ys_mild_sanitize(x)
  expect_identical(ans, "a _ b \\& c \\% d ^")
})


test_that("yspec_select [YSP-TEST-0120]", {
  spec <- ys_help$spec()
  x <- yspec_select_discrete(spec)
  expect_identical(
    names(x), 
    c("C", "SEQ", "EVID", "CP", "MDV", "BLQ", "PHASE", "STUDY", "RF")
  )
  x <- yspec_select_chr(spec)
  expect_identical(names(x), c("C", "SUBJ", "RF"))
})

test_that("expand on colon [YSP-TEST-0121]", {
  a <- yspec:::expand_names_on_colon(c("a", "m", "p"), letters)
  expect_false(a$any_bad)
  expect_identical(a$cols, c("a", "m", "p"))
  
  b <- yspec:::expand_names_on_colon(c("a:c", "m", "p"), letters)
  expect_false(b$any_bad)
  expect_identical(b$cols, c("a", "b", "c", "m", "p"))
  
  c <- yspec:::expand_names_on_colon(c("a:c", "m", "b"), letters)
  expect_false(c$any_bad)
  expect_identical(c$cols, c("a", "b", "c", "m"))
  
  d <- yspec:::expand_names_on_colon(c("A", letters[1:2]), LETTERS)
  expect_true(d$any_bad)
  expect_identical(d$bad_cols, c("a", "b"))
  
  e <- yspec:::expand_names_on_colon(letters, LETTERS)
  expect_true(e$any_bad)
  expect_identical(e$bad_cols, letters)
})
