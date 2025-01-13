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

test_that("ys get values - yspec", {
  spx <- ys_select(sp, AGE, STUDY, RF, WT, ALB, CP, PHASE)  
  x <- ys_get_values(spx)
  expect_is(x, "list")
  expect_named(x)
  expect_equal(names(x), c("STUDY", "RF", "CP", "PHASE"))
  expect_equivalent(spx$CP$values, x$CP)
})

test_that("ys get values - ycol", {
  x0 <- ys_get_values(sp)
  x <- ys_get_values(sp$CP)
  expect_is(x, "integer")
  expect_identical(x0$CP, x)
  expect_length(x, 4)
  expect_named(x)
  
  x <- ys_get_values(sp$CP, -Pugh2)
  expect_length(x, 3)
  expect_equivalent(x, c(0, 1, 3))
  
  x <- ys_get_values(sp$STUDY, -3)
  expect_length(x, 3)
  expect_equivalent(x, c(1, 2, 4))
  
  x <- ys_get_values(sp$STUDY, everything())
  expect_identical(x, x0$STUDY)
  
  sp$PHASE$values <- c(300, 200, 100)
  sp$PHASE$decode <- NULL
  
  x <- ys_get_values(sp$PHASE, -2)
  expect_equivalent(x, c(300, 100))
  
  x <- ys_get_values(sp$PHASE, -"200")
  expect_equivalent(x, c(300, 100))
  
  expect_error(ys_get_values(sp$PHASE, -200), "Location 200 doesn't exist")

  expect_error(ys_get_values(sp$STUDY, 0), "no values were selected")
})

test_that("ys mget values", {
  x00 <- ys_get_values(sp)
  x0 <- ys_mget_values(sp, STUDY = everything(), RF = -1, CP = 2)
  
  expect_identical(x00$STUDY, x0$STUDY)
  
  x <- ys_get_values(sp$RF, -1)
  expect_identical(x, x0$RF)
  
  x <- ys_get_values(sp$CP, 2)
  expect_equal(x, x0$CP)
})
