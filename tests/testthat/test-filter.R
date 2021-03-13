
library(yspec)
library(testthat)

context("test-filter")

test_that("select column filter - col", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, col %in% c("WT", "C", "ALB"))
  expect_equal(sort(names(a)), c("ALB", "C", "WT"))
})

test_that("select column filter - unit", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, unit=="kg")
  expect_equal(names(a), "WT")
})

test_that("select column filter - type", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, type=="character")
  b <- map(spec, "type")
  b <- spec[unlist(b)=="character"]
  expect_true(identical(a,b))
})

test_that("select column filter - continuous", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, continuous)
  b <- map(spec, "continuous")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - discrete", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, discrete)
  b <- map(spec, "discrete")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - do_lookup", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, do_lookup)
  b <- map(spec, "do_lookup")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - short", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, grepl("time", short))
  expect_equal(names(a), c("TAFD", "TAD"))
})

test_that("select column filter - values", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, length(values) > 1)
  b <- map(spec, "values")
  c <- map_int(b, length)
  d <- c[c > 1]
  expect_identical(names(a), names(d))
})

test_that("select column filter - decode", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, length(decode) > 1)
  b <- map(spec, "decode")
  c <- map_int(b, length)
  d <- c[c > 1]
  expect_identical(names(a), names(d))
})

test_that("select column filter - covariate", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, covariate)
  b <- map(spec, ~isTRUE(.x$dots$covariate))
  b <- spec[unlist(b)]
  expect_true(identical(a,b))  
})

test_that("dots", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, time_varying)
  b <- map(spec, ~isTRUE(.x$dots$time_varying))
  b <- spec[unlist(b)]
  expect_true(identical(a,b))  
})

test_that("nothing returned", {
  spec <- ys_help$spec()
  expect_warning(
    a <- ys_filter(
      spec, 
      unit == "tons"
    ), 
    regexp="no columns were selected"
  )
})
