library(yspec)
library(testthat)

context("test-define-help")

test_that("values eq decode output", {
  spec <- ys_help$spec()
  ans <- yspec:::pack_codes(spec$SEQ)
  expect_equal(ans, "0 = observation, 1 = dose")
  spec <- yspec:::test_spec_test("value-eq-decode.yaml")
  ans <- yspec:::pack_codes(spec$COL)
  expect_equal(ans, "a = e, b, c = z")
})
