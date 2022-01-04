library(yspec)
library(testthat)

context("test-prune")

test_that("ys_prune selects available columns", {
  data <- ys_help$data()
  spec <- ys_help$spec()
  data$STUDY <- NULL
  data$TAD <- NULL
  data$FOO <- 1
  data$BAR <- 2
  set.seed(1103)
  data <- data[, sample(names(data)), drop = FALSE]
  ans <- ys_prune(data, spec)
  spec2 <- ys_select(spec, -STUDY, -TAD)
  expect_identical(class(data), class(ans))
  expect_true(is.data.frame(ans))
  expect_equal(names(ans), names(spec2))
  expect_error(
    ys_prune(data.frame(a = 2), spec), 
    regexp = "there are no names common between"
  )
  ans <- ys_prune(data, spec, add = "BAR")
  expect_equal(names(ans), c(names(spec2), "BAR"))
  ans <- ys_prune(data, spec, add = "BLAH,FOO")
  expect_equal(names(ans), c(names(spec2), "FOO"))
  expect_message(
    ans <- ys_prune(data, spec, report = TRUE), 
    regexp = "Column not found: STUDY", 
    all = FALSE, fixed = TRUE
  )
  expect_message(
    ans <- ys_prune(data, spec, report = TRUE), 
    regexp = "Column not found: TAD", 
    all = FALSE, fixed = TRUE
  )
})