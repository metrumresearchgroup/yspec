library(yspec)
library(testthat)

context("test-character_last")

test_that("put character columns last, except comment_col [YSP-TEST-0008]", {
  spec <- yspec:::test_spec_test("character_last-issue-86.yml")
  type <- unname(purrr::map_chr(spec,"type"))
  n <- length(type)
  terminal_chr <- seq((n-2),n)
  expect_true(all(type[terminal_chr]=="character"))
  expect_equal(type[1],"character")
})


