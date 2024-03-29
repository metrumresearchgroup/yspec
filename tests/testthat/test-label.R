library(yspec)

context("test-label.R")

spec <- ys_help$spec()

test_that("label field exists [YSP-TEST-0054]", {
  expect_is(spec$TIME$label,"character")
  expect_is(spec$CRCL$label,"character")
  expect_null(spec$MDV$label)
})

test_that("get label [YSP-TEST-0055]", {
  x <- ys_get_label(spec$TIME,.aslist=TRUE)  
  expect_equal(x,spec$TIME$label)
  x <- ys_get_label(spec$ID)
  expect_equal(x,spec$ID$short)
})

test_that("required label [YSP-TEST-0056]", {
  file <- ys_help$file()  
  options(ys.require.label=TRUE)
  expect_error(ys_load(file))
  options(ys.require.label=NULL)
})
  
  
