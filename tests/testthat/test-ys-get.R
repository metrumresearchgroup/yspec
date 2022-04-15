library(yspec)
library(testthat)
library(dplyr)

context("test-ys-get")

sp <- ys_help$spec()

test_that("get unit [YSP-TEST-0128]", {
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

test_that("get label [YSP-TEST-0055]", {
  ans <- ys_get_label(sp)  
  expect_is(ans,"list")
  expect_named(ans)
  expect_equal(ans$TAD, "time after dose")
  expect_equal(ans$SCR, "serum creatinine")
  ans <- ys_get_unit(sp, .aslist = FALSE)
  expect_named(ans)
  expect_is(ans,"character")
})

test_that("get short [YSP-TEST-0130]", {
  ans <- ys_get_label(sp)  
  expect_is(ans,"list")
  expect_named(ans)
  expect_equal(ans$TAD, "time after dose")
  expect_equal(ans$SCR, "serum creatinine")
  ans <- ys_get_unit(sp, .aslist = FALSE)
  expect_named(ans)
  expect_is(ans,"character")
})

test_that("get short unit [YSP-TEST-0131]", {
  ans <- ys_get_short_unit(sp)  
  expect_is(ans,"list")
  expect_match(ans$WT, "(kg)", fixed = TRUE, all = FALSE)
  ans <- ys_get_short_unit(sp, parens = FALSE)
  expect_match(ans$WT, " kg$", fixed = FALSE, all = FALSE)
})

test_that("short is in title case [YSP-TEST-0132]", {
  sh1 <- ys_get_short(sp)
  sh2 <- ys_get_short(sp, title_case = TRUE)
  expect_identical(sh1$WT, "weight")
  expect_identical(sh2$WT, "Weight")
})

test_that("short is limited to n characters [YSP-TEST-0133]", {
  sh1 <- ys_get_short(sp)
  sh2 <- ys_get_short(sp, short_max = 2)
  expect_identical(sh1$WT, "weight")
  expect_identical(sh2$WT, "WT")
})




