
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

test_that("flags error when col not found", {
  expect_error(
    yspec:::test_spec_test("flags-error.yaml"), 
    regexp = "names not found in spec:\n - b\n - c"
  )
})

test_that("flags - warn when exists and overwrite", {
  expect_warning( 
    yspec:::test_spec_test("flags-warn-exists.yml"), 
    regexp = "but is not logical type"
  )
})

test_that("flags are propagated from lookup file", {
  spec <- ys_help$spec()
  expect_true(spec$CP$dots$covariate)
  just_cp <- ys_filter(spec, covariate)
  expect_true("CP" %in% names(just_cp))
  expect_false(spec$CP$dots$updated_from_lookup)
  expect_true(spec$CP$dots$came_from_lookup)
})
