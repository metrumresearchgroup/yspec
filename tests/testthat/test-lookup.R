
library(testthat)
library(yspec)

context("test-lookup")

test_that("lookup with internal db [YSP-TEST-0073]", {
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

test_that("lookup without internal db [YSP-TEST-0074]", {
  file <- system.file("spec", "lookup", "spec_nodb.yml",package = "yspec")
  expect_warning(ys_load(file), "not find lookup data for AGE")
  spec <- suppressWarnings(ys_load(file))
  expect_equal(spec$A$values, 1)
  expect_equal(spec$B$values, 2)
  expect_equal(spec$C$short, "lookup")
  expect_equal(spec$D$short, "lookup")
  expect_equal(spec$E$short, "F")
  expect_equal(spec$WT$short, "wait")
})

# issue #69
test_that("dots are inherited when dots aren't already existing [YSP-TEST-0075]", {
  spec <- ys_help$spec()
  yam <- yaml::yaml.load_file(ys_help$file())
  expect_is(yam$TIME$dots, "NULL")
  expect_is(spec$TIME$dots, "list")
  expect_is(spec$TIME$dots$timecol, "logical")
  spect <- as.data.frame(ys_filter(spec, isTRUE(timecol)))
  expect_equal(nrow(spect), 3)
  expect_equal(spect$name, c("TIME", "TAFD", "TAD"))
  expect_equal(spect$source, c("look", ".", "."))
})
