library(yspec)
library(testthat)

context("test-character_last")

.sp <- yspec:::test_spec_test

test_that("put character columns last, except comment_col", {
  spec <- .sp("character_last-issue-86.yml")
  type <- unname(map_chr(spec,"type"))
  n <- length(type)
  terminal_chr <- seq((n-2),n)
  expect_true(all(type[terminal_chr]=="character"))
  expect_equal(type[1],"character")
})


