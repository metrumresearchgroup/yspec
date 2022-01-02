library(yspec)
library(testthat)

context("text-prune")

test_that("ys_prune selects available columns", {
  data <- ys_help$data()
  spec <- ys_help$spec()
  data$STUDY <- NULL
  data$TAD <- NULL
  data$FOO <- 1
  data$BAR <- 2
  ans <- ys_prune(data, spec)
  spec2 <- ys_select(spec, -STUDY, -TAD)
  expect_identical(class(data), class(ans))
  expect_true(is.data.frame(ans))
  expect_equal(names(ans), names(spec2))
  expect_error(
    ys_prune(data.frame(a = 2), spec), 
    regexp = "there are no names common between"
  )
})
