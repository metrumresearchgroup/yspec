
library(yspec)
library(testthat)

context("test-select")

test_that("select column subset [YSP-TEST-0084]", {
  spec <- ys_help$spec()
  # spec2 <- ys_select(spec)
  # expect_identical(spec,spec2)
  spec3 <- ys_select(spec, WT, AGE, ALB)
  expect_identical(names(spec3), c("WT", "AGE", "ALB"))
  expect_is(spec3, "yspec [YSP-TEST-0086]")
  expect_error(ys_select(spec, kyle))
})

test_that("select with no matching names returns zero-length yspec [YSP-TEST-0085]", {
  spec <- ys_help$spec()
  spec <- ys_select(spec, character(0))
  expect_length(spec, 0)
  expect_is(spec, "yspec [YSP-TEST-0086]")
  m <- get_meta(spec)
  expect_is(m, "list")
  expect_true(length(m) > 0)
})

