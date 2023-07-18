library(yspec)
library(testthat)

context("test-col_factor")

spec <- load_spec_ex()

test_that("factor is generated from spec [YSP-TEST-0020]", {
  
  data <- data.frame(WT  = rnorm(30), 
                     SEX = rbinom(30, 1, 0.5), 
                     STUDY = sample(c(100, 202,303,203), size = 30, replace = TRUE )
  )
  data <- ys_add_factors(data, spec, SEX, STUDY)
  
  expect_is(data$SEX_f, "factor")
  expect_equal(levels(data$SEX_f), c("male", "female"))
})

test_that("factor is generated from ycol object [YSP-TEST-0021]", {
  sex <- rbinom(100, 1, 0.5)
  fact <- ys_make_factor(sex, spec$SEX)
  expect_is(fact, "factor")
  expect_equal(levels(fact), c("male", "female"))
  
})

test_that("error making factor from continuous data [YSP-TEST-0022]", {
   expect_error(yspec_make_factor(c(1,2,3,4), spec$WT))
})

test_that("make all factors [YSP-TEST-0023]", {
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

test_that("ys_add_factors aliases yspec_add_factors [YSP-TEST-0024]", {
  dat <- ys_help$data()
  sp <- ys_help$spec()
  a <- yspec_add_factors(dat,sp)
  b <- ys_add_factors(dat,sp)
  expect_identical(a,b)
})

test_that("NA handling by ys_add_factors [YSP-TEST-0025]", {
  dat1 <- ys_help$data()
  sp <- ys_help$spec()
  dat1$RF[c(3,10,30)] <- NA_character_
  dat2 <- dat1
  a <- yspec_add_factors(dat1, sp, RF)
  expect_identical(levels(a$RF_f), sp$RF$decode)
  b <- yspec_add_factors(dat2, sp, RF, .missing = "Missing")
  expect_identical(levels(b$RF_f), c(sp$RF$decode, "Missing"))
})

test_that("OK to have missing or extra cols in .data [YSP-TEST-0026]", {
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

test_that("tidyselect semantics when identifying columns for factor [YSP-TEST-0027]", {
  data <- ys_help$data()
  spec <- ys_help$spec()
  
  ans <- ys_add_factors(data, spec, tidyselect::contains("RF"))
  expect_true("RF_f" %in% names(ans))
  
  ans <- ys_add_factors(data, spec, BLQ:STUDY)
  expect_true("BLQ_f" %in% names(ans))
  expect_true("PHASE_f" %in% names(ans))
  expect_true("STUDY_f" %in% names(ans))
})

test_that("ys_factors converts originals to factors", {
  data <- ys_help$data()  
  spec <- ys_help$spec()
  
  expect_true(is.numeric(data$EVID))
  expect_false("EVID_v" %in% names(data))
  
  data <- ys_factors(data, spec, EVID)
  expect_is(data$EVID, "factor")
  expect_equal(levels(data$EVID), spec$EVID$decode)
  
  expect_true("EVID_v" %in% names(data))
  
  expect_error(ys_factors(data, spec, KYLE), 
               "Column `KYLE` doesn't exist.")
})

test_that("ys_factors retains original values by default", {
  data0 <- ys_help$data()  
  spec <- ys_help$spec()

  expect_false("EVID_v" %in% names(data0))
  
  data <- ys_factors(data0, spec, EVID)

  expect_true("EVID_v" %in% names(data))
  expect_equal(unique(data$EVID_v), c(1,0))
  
  data <- ys_factors(data0, spec, EVID, .suffix = "values")
  expect_true("EVIDvalues" %in% names(data))
})

test_that("ys_factors discard original values", {
  data <- ys_help$data()  
  spec <- ys_help$spec()
  
  data1 <- ys_factors(data, spec, .keep_values = FALSE)
  expect_identical(names(data), names(data1))
  
  expect_true(is.numeric(data$EVID))
  expect_true(is.factor(data1$EVID))
  
  data2 <- ys_factors(data, spec, .suffix = NULL)
  expect_identical(data1, data2)
})

test_that("ys_factors will convert everything", {
  data <- ys_help$data()  
  spec <- ys_help$spec()
  
  decode  <- purrr::map(spec, "decode")
  mkf     <- purrr::map(spec, "make_factor")
  decode  <- purrr::compact(decode)
  mkf     <- purrr::compact(mkf)
  ndecode <- length(decode) + length(mkf)
  
  data <- ys_factors(data, spec)
  values <- names(data)[grep("_v", names(data), fixed = TRUE)]
  expect_equal(length(values), ndecode)
  
  nw <- data[, sapply(data, is.factor)]
  expect_equal(ncol(nw), ndecode)
})

test_that("call ys_factors more than once on a data frame", {
  data <- ys_help$data()  
  spec <- ys_help$spec()
  
  data2 <- ys_factors(data, spec)
  data3 <- ys_factors(data2, spec)
  expect_identical(data2, data3)
  
  labs <- names(data)
  data4 <- ys_factors(data, spec, EVID)
  expect_identical(names(data4), c(labs, "EVID_v"))
  data5 <- ys_factors(data4, spec, CP, EVID)
  expect_identical(names(data5), c(labs, "EVID_v", "CP_v"))
  
  data6 <- ys_factors(data5, spec)
  data7 <- ys_factors(data6, spec)
  expect_identical(data6, data7)
})
