
library(testthat)
library(yspec)

context("test-lookup")

test_that("lookup with internal db", {
  file <- system.file("spec", "lookup", "spec.yml",package = "yspec")
  spec <- ys_load(file)
  
  expect_equal(spec$A$values,1)
  expect_equal(spec$B$values,2)
  expect_equal(spec$C$short,"lookup")
  expect_equal(spec$D$short,"lookup")
  expect_equal(spec$E$short,"F")
  expect_equal(spec$AGE$unit, "years")
  expect_equal(spec$WT$short, "wait")
})

test_that("lookup without internal db", {
  file <- system.file("spec", "lookup", "spec_nodb.yml",package = "yspec")
  expect_warning(ys_load(file), "not find lookup data for AGE")
  spec <- suppressWarnings(ys_load(file))
  expect_equal(spec$A$values,1)
  expect_equal(spec$B$values,2)
  expect_equal(spec$C$short,"lookup")
  expect_equal(spec$D$short,"lookup")
  expect_equal(spec$E$short,"F")
  expect_equal(spec$WT$short, "wait")
})


