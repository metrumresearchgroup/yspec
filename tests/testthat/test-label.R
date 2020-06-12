library(yspec)
library(purrr)

context("test-label.R")

spec <- ys_help$spec()

test_that("label field exists", {
  expect_is(spec$TIME$label,"character")
  expect_is(spec$CRCL$label,"character")
  expect_null(spec$MDV$label)
})

test_that("get label", {
  x <- get_label(spec$TIME)  
  expect_equal(x,spec$TIME$label)
  x <- get_label(spec$ID)
  expect_equal(x,spec$ID$short)
})

