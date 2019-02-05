library(yspec)
library(testthat)
library(dplyr)

context("test-glue")

yt <- test_spec_list

test_that("TeX code is properly glued", {
  l <- list(FOO = list(unit = "<<mug>>"))
  set <- list(glue = list(mug = "$\\mu$g"))
  sp <- yt(l,set)
  txt <- unname(x_table(sp))
  ind <- which(grepl("unit", txt))
  look <- txt[ind]
  expect_true(grepl("$\\mu$g", look, fixed = TRUE))
})

