
library(yspec)
library(testthat)

context("test-define")

test_that("ys_table returns report table code [YSP-TEST-0127]", {
  spec <- ys_help$spec()
  tex <- ys_table(spec)
  expect_is(tex, "character")
  expect_match(tex[3], "VARIABLE")
  expect_match(tex, "Continued on next page", all = FALSE)
  tex <- ys_table(spec, widths_ = c(1.234,999,44,0))
  expect_match(tex, "1.234", fixed = TRUE, all = FALSE)
  expect_match(tex, "999", fixed = TRUE, all = FALSE)
})

