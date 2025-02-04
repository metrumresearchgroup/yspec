
library(yspec)
library(testthat)

context("test-flags")

test_that("set flags in SETUP__ [YSP-TEST-0041]", {
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

test_that("flags error when col not found [YSP-TEST-0042]", {
  expect_error(
    yspec:::test_spec_test("flags-error.yaml"), 
    regexp = "names not found in spec:\n - b\n - c"
  )
})

test_that("flags - warn when exists and overwrite [YSP-TEST-0043]", {
  expect_warning( 
    yspec:::test_spec_test("flags-warn-exists.yml"), 
    regexp = "but is not logical type"
  )
})

test_that("flags are propagated from lookup file [YSP-TEST-0044]", {
  spec <- ys_help$spec()
  expect_true(spec$CP$dots$covariate)
  just_cp <- ys_filter(spec, covariate)
  expect_true("CP" %in% names(just_cp))
  expect_false(spec$CP$dots$updated_from_lookup)
  expect_true(spec$CP$dots$came_from_lookup)
})

test_that("flags are extracted as list", {
  spec <- ys_help$spec()
  
  met <- pull_meta(spec, "flags")
  x <- ys_flags(spec)
  expect_identical(names(met), names(x))
  
  x <- ys_flags(spec, covariate, times)
  expect_identical(names(x), c("covariate", "times"))
  expect_identical(x$covariate, met$covariate)
  expect_identical(x$times, met$times)
  
  x <- ys_flags(spec, covariate:cat)
  expect_identical(x, met)
  
  expect_error(ys_flags(spec, abc), "Column `abc` doesn't exist.")
})

test_that("flags are extracted as character vector", {
  spec <- ys_help$spec()
  
  met <- pull_meta(spec, "flags")
  
  x <- ys_flags_chr(spec)
  met_chr <- unlist(met, use.names = FALSE)
  expect_identical(x, met_chr)
  
  x <- ys_flags_chr(spec, times, covariate)
  expect_null(names(x))
  met_chr <- unlist(met[c("times", "covariate")], use.names = FALSE)
  expect_identical(x, met_chr)
})

test_that("select from spec via flags", {
  spec <- ys_help$spec()
  
  sspec <- ys_select_fl(spec, covariate, nm)
  cols <- ys_flags_chr(spec, covariate, nm)
  
  expect_identical(names(sspec), cols)
  
  expect_error(ys_select_fl(spec, abc), "Column `abc` doesn't exist.")
})

test_that("No result with selection helper returns zero-length list", {
  spec <- ys_help$spec()  
  ans <- ys_flags(spec, contains("foobert"))
  expect_is(ans, "list")
  expect_length(ans, 0)
  
  ans <- ys_flags(spec, contains("foobert"), contains("covar"))
  expect_is(ans, "list")
  expect_length(ans, 1)
  expect_equal(names(ans), "covariate")
})

test_that("Rename when selecting flags", {
  spec <- ys_help$spec()
  ans <- ys_flags(spec, nonmem = nm, covariates = covariate)
  expect_is(ans, "list")
  expect_length(ans, 2)
  expect_equal(names(ans), c("nonmem", "covariates"))
})
