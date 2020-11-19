
library(yspec)
library(testthat)

context("test-select")

test_that("select column subset", {
  spec <- ys_help$spec()
  spec2 <- ys_select(spec)
  expect_identical(spec,spec2)
  spec3 <- ys_select(spec, WT, AGE, ALB)
  expect_identical(names(spec3), c("WT", "AGE", "ALB"))
  expect_is(spec3, "yspec")
  expect_error(ys_select(spec, kyle))
})

