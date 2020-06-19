library(yspec)
library(testthat)
library(dplyr)

context("test-ys-get")

sp <- ys_help$spec()

test_that("get unit", {
  ans <- ys_get_unit(sp)  
  expect_is(ans,"list")
  expect_named(ans)
  expect_equal(ans$TAD, "hours")
  expect_equal(ans$SCR, "mg/dL")
  ans <- ys_get_unit(sp, .aslist = FALSE)
  expect_named(ans)
  expect_is(ans,"character")
  ans <- ys_get_unit(sp, parens = TRUE)
  expect_equal(ans$WT, "(kg)")
  expect_equal(ans$ALB, "(g/dL)")
})

test_that("get label", {
  ans <- ys_get_label(sp)  
  expect_is(ans,"list")
  expect_named(ans)
  expect_equal(ans$TAD, "time after dose")
  expect_equal(ans$SCR, "serum creatinine")
  ans <- ys_get_unit(sp, .aslist = FALSE)
  expect_named(ans)
  expect_is(ans,"character")
})

test_that("get short", {
  ans <- ys_get_label(sp)  
  expect_is(ans,"list")
  expect_named(ans)
  expect_equal(ans$TAD, "time after dose")
  expect_equal(ans$SCR, "serum creatinine")
  ans <- ys_get_unit(sp, .aslist = FALSE)
  expect_named(ans)
  expect_is(ans,"character")
})


