library(yspec)
library(testthat)

context("test-col_factor")

spec <- load_spec_ex()

test_that("factor is generated from spec", {
  
  data <- data.frame(WT  = rnorm(30), 
                     SEX = rbinom(30, 1, 0.5), 
                     STUDY = sample(c(100, 202,303,203), size = 30, replace = TRUE )
  )
  data <- ys_add_factors(data, spec, SEX, STUDY)
  
  expect_is(data$SEX_f, "factor")
  expect_equal(levels(data$SEX_f), c("male", "female"))
})

test_that("factor is generated from ycol object", {
  sex <- rbinom(100, 1, 0.5)
  fact <- ys_make_factor(sex, spec$SEX)
  expect_is(fact, "factor")
  expect_equal(levels(fact), c("male", "female"))
  
})

test_that("error making factor from continuous data", {
   expect_error(yspec_make_factor(c(1,2,3,4), spec$WT))
})

test_that("make all factors", {
  dat <- ys_help$data()
  sp <- ys_help$spec()
  
  has_values <- c("C", "SEQ", "EVID", "CP", "MDV", "BLQ", "PHASE", "STUDY", "RF")
  
  before <- names(dat)
  dat <- ys_add_factors(dat, sp)
  after <- names(dat)
  diff <- setdiff(after,before)
  expect_identical(diff, paste0(has_values,"_f"))
  cl <- sapply(dat[,diff], class)
  expect_true(all(cl=="factor"))
})

test_that("ys_add_factors aliases yspec_add_factors", {
  dat <- ys_help$data()
  sp <- ys_help$spec()
  a <- yspec_add_factors(dat,sp)
  b <- ys_add_factors(dat,sp)
  expect_identical(a,b)
})

test_that("NA handling by ys_add_factors", {
  dat1 <- ys_help$data()
  sp <- ys_help$spec()
  dat1$RF[c(3,10,30)] <- NA_character_
  dat2 <- dat1
  a <- yspec_add_factors(dat1, sp, RF)
  expect_identical(levels(a$RF_f), sp$RF$decode)
  b <- yspec_add_factors(dat2, sp, RF, .missing = "Missing")
  expect_identical(levels(b$RF_f), c(sp$RF$decode, "Missing"))
})

test_that("OK to have missing or extra cols in .data", {
  dat1 <- dat2 <- ys_help$data()
  sp <- ys_help$spec()
  dat2$RF <- NULL
  dat2$CP <- NULL
  dat2$FOO <- 1
  dat1 <- ys_add_factors(dat1, sp)
  dat2 <- ys_add_factors(dat2, sp)
  diff <- setdiff(names(dat1), names(dat2))
  expect_is(dat1, "data.frame")
  expect_is(dat2, "data.frame")
  expect_equal(sort(diff), sort(c("RF_f", "CP_f", "RF", "CP")))
})
