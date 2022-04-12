library(yspec)
library(testthat)

context("test-exspec")

test_that("load_spec_ex [YSP-TEST-0034]", {
  expect_is(load_spec_ex(), "yspec")
  expect_is(ys_load(file_spec_ex()), "yspec")
})

test_that("load_proj_ex [YSP-TEST-0035]", {
  expect_is(load_proj_ex(), "yproj")
  expect_is(load_spec_any(file_proj_ex()), "yproj")
})

