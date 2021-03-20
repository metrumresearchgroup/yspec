
library(yspec)
library(testthat)
library(purrr)

context("test-tidy")

test_that("select column filter - col", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, col %in% c("WT", "C", "ALB"))
  expect_is(a, "yspec")
  expect_equal(sort(names(a)), c("ALB", "C", "WT"))
})

test_that("select column filter - unit", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, unit=="kg")
  expect_is(a, "yspec")
  expect_equal(names(a), "WT")
})

test_that("select column filter - type", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, type=="character")
  expect_is(a, "yspec")
  b <- purrr::map(spec, "type")
  b <- spec[unlist(b)=="character"]
  expect_true(identical(a,b))
})

test_that("select column filter - continuous", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, continuous)
  expect_is(a, "yspec")
  b <- purrr::map(spec, "continuous")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - discrete", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, discrete)
  expect_is(a, "yspec")
  b <- purrr::map(spec, "discrete")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - do_lookup", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, do_lookup)
  expect_is(a, "yspec")
  b <- purrr::map(spec, "do_lookup")
  b <- spec[unlist(b)]
  expect_true(identical(a,b))
})

test_that("select column filter - short", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, grepl("time", short))
  expect_is(a, "yspec")
  expect_equal(names(a), c("TAFD", "TAD"))
})

test_that("select column filter - values", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, length(values) > 1)
  expect_is(a, "yspec")
  b <- map(spec, "values")
  c <- map_int(b, length)
  d <- c[c > 1]
  expect_identical(names(a), names(d))
})

test_that("select column filter - decode", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, length(decode) > 1)
  expect_is(a, "yspec")
  b <- map(spec, "decode")
  c <- map_int(b, length)
  d <- c[c > 1]
  expect_identical(names(a), names(d))
})

test_that("select column filter - covariate", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, covariate)
  expect_is(a, "yspec")
  b <- map(spec, ~isTRUE(.x$dots$covariate))
  b <- spec[unlist(b)]
  expect_true(identical(a,b))  
})

test_that("select column filter - dots", {
  spec <- ys_help$spec()
  a <- ys_filter(spec, time_varying)
  expect_is(a, "yspec")
  b <- map(spec, ~isTRUE(.x$dots$time_varying))
  b <- spec[unlist(b)]
  expect_true(identical(a,b))  
})

test_that("select column filter - nothing returned", {
  spec <- ys_help$spec()
  expect_warning(
    a <- ys_filter(
      spec, 
      unit == "tons"
    ), 
    regexp="no columns were selected"
  )
})

test_that("filter with negate", {
  spec <- ys_help$spec()
  ans <- ys_filter(spec, covariate & col != "WT")
  expect_is(ans, "yspec")
  expect_false("WT" %in% names(ans))
})

test_that("filter with in", {
  spec <- ys_help$spec()
  ans <- ys_filter(spec, col %in% c("WT", "SCR", "ALB"))
  expect_is(ans, "yspec")
  expect_identical(sort(names(ans)), sort(c("WT", "ALB", "SCR")))
})

test_that("filter from parent frame", {
  yes <- "abc"
  spec <- ys_help$spec()
  spec2 <- ys_filter(spec, yes=="abc")
  expect_equivalent(spec,spec2)
})

test_that("filter from defaults", {
  def <- list(no = "xyz")
  spec <- ys_help$spec()
  spec2 <- ys_filter(spec, no=="xyz", .default = def)
  expect_equivalent(spec,spec2)
})

test_that("rename yspec columns", {
  spec <- ys_help$spec() 
  spec <- ys_select(spec, WT, BMI, STUDY)
  spec <- ys_rename(spec, Wt = WT, STDY = "STUDY")
  expect_is(spec, "yspec")
  expect_equal(names(spec), c("Wt", "BMI", "STDY"))
  cols <- purrr::map_chr(spec, "col")
  expect_equal(unname(cols), names(spec))
})

test_that("select spec columns with rename", {
  spec <- ys_help$spec()
  ans <- ys_select(spec, Wt = WT, AGE, bmi = BMI)
  expect_is(ans, "yspec")
  expect_equal(names(ans), c("Wt", "AGE", "bmi"))
  cols <- purrr:::map_chr(ans, "col")
  expect_equal(unname(cols), names(ans))
})

test_that("join two or more spec objects", {
  spec <- ys_help$spec()
  a <- ys_select(spec, WT, BMI)
  b <- ys_select(spec, EVID, CMT) 
  c <- ys_select(spec, TIME, TAD)
  d <- ys_select(spec, HT, WT, BMI, AGE)
  ans <- ys_join(a, b, c) 
  expect_is(ans, "yspec")
  expect_equal(names(ans), c(names(a), names(b), names(c)))
  ans <- ys_join(a, d)
  expect_is(ans, "yspec")
  expect_equal(names(ans), c("WT", "BMI", "HT", "AGE"))
})

test_that("fill dots", {
  spec <- ys_help$spec()
  spec2 <- ys_fill_dots(spec, a = TRUE, b = FALSE)
  expect_is(spec2, "yspec")
  expect_false(identical(spec, spec2))
  a <- map_lgl(spec2, ~.x$dots$a)
  b <- map_lgl(spec2, ~.x$dots$b)
  expect_true(all(a))
  expect_false(any(b))
  expect_null(spec$TIME$dots$covariate)
  spec3 <- ys_fill_dots(spec, covariate = FALSE)
  expect_true(spec3$WT$dots$covariate)
  expect_false(spec3$TIME$dots$covariate)
})
