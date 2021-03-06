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

