library(yspec)
library(testthat)
library(dplyr)

context("test-nm_input")

sp <- ys_help$spec()

test_that("input text is formed from spec", {
  txt <- nm_input(sp, cat = FALSE)
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

