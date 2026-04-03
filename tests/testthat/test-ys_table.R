
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

test_that("ns argument sets namespace prior to generating table", {
  spec <- ys_help$spec()
  # first, with default namespace
  tex <- ys_table(spec)
  tex <- tex[grepl("LDOS", tex)]
  expect_match(tex, "unit: milligram", fixed = TRUE)
  # now, remove define
  tex <- ys_table(spec, ns = "tex")
  tex1 <- tex[grepl("LDOS", tex)]
  expect_match(tex1, "unit: mg", fixed = TRUE)
  tex2 <- tex[grepl("dependent", tex)]
  expect_match(tex2, "unit: $\\mu$g", fixed = TRUE)
  # Pass NULL
  tex <- ys_table(spec, ns = NULL)
  tex <- tex[grepl("dependent", tex)]
  expect_match(tex, "unit: micrograms/L", fixed = TRUE)
})
