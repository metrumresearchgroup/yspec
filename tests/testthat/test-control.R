library(yspec)
library(testthat)

context("test-control.R")

test_that("control defaults", {
  ans <- yspec:::ys_control_defaults()
  expect_length(ans, 3)
  expect_true("max_nchar_label" %in% names(ans))
  expect_true("max_nchar_col" %in% names(ans))
  expect_true("max_nchar_short" %in% names(ans))
  expect_equal(ans[["max_nchar_label"]], 40)
  expect_equal(ans[["max_nchar_short"]], 40)
  expect_equal(ans[["max_nchar_col"]], 8)
})

test_that("wrong types are detected", {
  meta <- list(max_nchar_label = "forty")
  expect_error(
    yspec:::get_spec_control(meta), 
    msg = "meta field max_nchar_label has wrong type (required: numeric)"
  )
  meta <- list(max_nchar_short = TRUE)
  expect_error(
    yspec:::get_spec_control(meta), 
    msg = "meta field max_nchar_short has wrong type (required: numeric)"
  )
  meta <- list(max_nchar_col = list)
  expect_error(
    yspec:::get_spec_control(meta), 
    msg = "meta field max_nchar_col has wrong type (required: numeric)"
  )
})

