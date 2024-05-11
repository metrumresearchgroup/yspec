library(yspec)
library(testthat)
library(dplyr)

context("test-nm_input")

sp <- ys_help$spec()

test_that("input text is formed from spec", {
  txt <- nm_input(sp, .cat = FALSE)
  expect_is(txt, "character")
  expect_equal("$INPUT", txt[1])
  expect_length(txt, 3)
  expect_match(txt, "RF=DROP", all = FALSE)
})

test_that("input text width is controlled", {
  txt <- nm_input(sp, .cat = FALSE, .width = 30)
  expect_length(txt, 6)
})

test_that("input columns can be droped", {
  txt <- nm_input(sp, .cat = FALSE, .drop = "EVID,HT")
  expect_match(txt, "RF=DROP", all = FALSE)
  expect_match(txt, "EVID=DROP", all = FALSE)
  expect_match(txt, "HT=DROP", all = FALSE)
})

test_that("input columns can be renamed", {
  txt <- nm_input(sp, .cat = FALSE, FOO = BMI, BAR = NUM)
  expect_match(txt, "FOO=BMI", all = FALSE)
  expect_match(txt, "BAR=NUM", all = FALSE)
})

test_that("duplicate input columns are not allowed", {
  expect_error(
    nm_input(sp, .cat = FALSE, BMI = NUM), 
    regexp="Duplicated input names"
  )
})

test_that("dropped columns must exist", {
  expect_error(nm_input(sp, .drop = "KYLE"), regexp="Can't drop")
})

test_that("rename columns must exist", {
  expect_error(nm_input(sp, KYLE = BARON), regexp="Can't rename")
})

test_that("don't rename dropped column", {
  txt <- nm_input(sp, .cat = FALSE, FOO = BMI, .drop = "BMI")
  expect_match(txt, "BMI=DROP ", all = FALSE)
})

test_that("ok to reuse name that was dropped", {
    txt <- nm_input(sp, .cat = FALSE, CP = BMI, .drop = "CP")
    expect_match(txt, "CP=DROP", all = FALSE)
    expect_match(txt, "CP=BMI", all = FALSE)
})

test_that("long input text is formed from spec [YSP-TEST-0081]", {
  txt <- nm_input(sp, .cat = FALSE, .long = TRUE, .decodes = TRUE)
  expect_is(txt, "character")
  n <- length(txt[substr(txt, 1, 1) != " "])
  expect_equal(n-1, length(sp))
  expect_equal(txt[1], "$INPUT")
  ans <- substr(txt[5], 1, 9)
  expect_equal(ans, "SUBJ=DROP")
  tx <- strsplit(txt[2:length(txt)], " ; ")
  tx <- map(tx, 1)
  tx <- map_chr(tx,trimws)
  tx <- tx[tx!=""]
  na <- gsub("=DROP", "",tx, fixed = TRUE)
  expect_identical(na, names(sp))
})

test_that("long input text includes just short by default", {
  txt <- nm_input(sp, .cat = FALSE, .long = TRUE)
  expect_length(txt, length(sp) + 1)
  n <- which(names(sp)=="EVID")
  x <- strsplit(txt[[n+1]], ";")[[1]]
  x <- trimws(x)
  expect_equal(x[2], sp$EVID$short)
})

test_that("long input text can include decodes", {
  txt <- nm_input(sp, .cat = FALSE, .long = TRUE, .decodes = TRUE)
  expect_length(txt, 37)
  expect_match(txt[30], "0 = non-missing")
})
