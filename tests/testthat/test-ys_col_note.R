library(yspec)
library(testthat)

context("test-col-note")

spec <- ys_help$spec()

test_that("ys_col_note generates column definitions [YSP-TEST-0123]", {
  ans <- ys_col_note(spec, WT, AAG, STUDY, .to_string = FALSE)
  expect_equal(length(ans), 3)
  ans <- ys_col_note(spec, CRCL, AAG, STUDY)
  expect_equal(length(ans), 1)
  expect_is(ans, "character")
  expect_match(ans, "CRCL: CRCL", fixed = TRUE)
  expect_match(ans, "AAG: alpha-1-acid glycoprotein", fixed = TRUE)
  expect_match(ans, "STUDY: study number", fixed = TRUE)
})

test_that("ys_col_note can pull label [YSP-TEST-0124]", {
  ans <- ys_col_note(spec, CRCL, AAG, .type = "label")
  expect_match(ans, "CRCL: creatinine clearance", fixed = TRUE)
  expect_match(ans, "AAG: alpha-1-acid glycoprotein", fixed = TRUE)
})

test_that("ys_col_note can customize separators [YSP-TEST-0125]", {
  ans <- ys_col_note(spec, CRCL, .sep = "-")
  expect_equal(ans, "CRCL-CRCL")
  ans <- ys_col_note(spec, CRCL, MDV, .collapse = "+", .sep = "-")
  expect_equal(ans, "CRCL-CRCL+MDV-MDV")
})

test_that("ys_col_note can render title case [YSP-TEST-0126]", {
  ans <- ys_col_note(spec, STUDY, .title_case = TRUE)
  expect_equal(ans, "STUDY: Study Number")
})
