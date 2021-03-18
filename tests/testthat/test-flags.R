
library(yspec)
library(testthat)

context("test-flags")

test_that("set flags in SETUP__", {
  sp <- yspec:::test_spec_test("flags.yaml")
  expect_true(sp$h$dots$set1)
  expect_true(sp$i$dots$set1)
  expect_true(sp$j$dots$set1)
  
  expect_true(sp$b$dots$set2)
  expect_true(sp$c$dots$set2)
  expect_true(sp$d$dots$set2)
  expect_true(sp$k$dots$set2)
  
  a <- ys_filter(sp, set2)
  b <- ys_filter(sp, set3)
  expect_identical(a,b)
})

test_that("flags error", {
  expect_error(
    yspec:::test_spec_test("flags-error.yaml"), 
    msg = "names not found in spec:\n- b\n-c"
  )
})

