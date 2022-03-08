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
  ans <- ys_prune(data, spec, BAR)
  expect_equal(names(ans), c(names(spec2), "BAR"))
  ans <- ys_prune(data, spec, BAR, FOO)
  expect_equal(names(ans), c(names(spec2), "BAR", "FOO"))
  expect_message(
    ans <- ys_prune(data, spec, .report = TRUE), 
    regexp = "Column not found: STUDY", 
    all = FALSE, fixed = TRUE
  )
  expect_message(
    ans <- ys_prune(data, spec, .report = TRUE), 
    regexp = "Column not found: TAD", 
    all = FALSE, fixed = TRUE
  )
})

test_that("prune can take all columns with re-ordering", {
  spec <- ys_help$spec() 
  data <- ys_help$data()
  data$WT <- NULL
  data$SCR <- NULL  
  lbl0 <- names(data)
  data$b <- 5
  data$a <- 2
  data <- data[, sample(seq(ncol(data)))]
  ans <- ys_prune(data, spec, tidyselect::everything())  
  lbl <- names(ans)
  n <- length(lbl)
  expect_equal(lbl[n], "b")
  expect_equal(lbl[n-1], "a")
  expect_equal(lbl[-c(n, n-1)], lbl0)
})
