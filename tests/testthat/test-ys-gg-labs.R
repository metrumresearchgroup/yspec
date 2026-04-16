library(yspec)
library(testthat)
library(dplyr)

skip_if_not_installed("ggplot2")

context("test-ys-gg-labs")

data <- ys_help$data()
spec <- ys_help$spec()
spec <- update_short(spec, DV = "Concentration", TIME = "Time")
spec$DV$unit <- "ng/mL"
labs <- list(
  TIME = "Time after first dose"
)

p0 <- 
  ggplot2::ggplot(data, ggplot2::aes(TIME, DV)) + 
  ggplot2::geom_point()

test_that("label x- and y- from the spec", {
  expect_null(p0@labels$x)
  expect_null(p0@labels$y)

  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$x, "Time (hour)")
  expect_equal(p@labels$y, "Concentration (ng/mL)")
})

test_that("labs overrides spec for x- and y-", { 
  p <- p0 + ys_gg_labs(spec, labs)
  expect_equal(p@labels$x, "Time after first dose")
  expect_equal(p@labels$y, "Concentration (ng/mL)")
})

test_that("force in x- or y-", { 
  p <- p0 + ys_gg_labs(spec, labs, x = "A", y = "B")
  expect_equal(p@labels$x, "A")
  expect_equal(p@labels$y, "B")
})

test_that("transformed x- or y- is ignored", {
  p0 <- 
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) + 
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$x, "Time (hour)")
  expect_equal(p@labels$y, "log(DV)")
})

test_that("transformed x- or y- in labs", {
  labs[["log(DV)"]] <- "log-transformed DV"
  p0 <- 
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) + 
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec, labs)
  expect_equal(p@labels$x, "Time after first dose")
  expect_equal(p@labels$y, "log-transformed DV")
})

test_that("factor x- or y- is passed through", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(factor(CP), DV)) +
    ggplot2::geom_boxplot()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$x, "Child-Pugh score")
  expect_equal(p@labels$y, "Concentration (ng/mL)")
})

test_that("label colour", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$colour, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, colour = "test colour")
  expect_equal(p@labels$colour, "test colour")
})

test_that("label fill", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, fill = factor(CP))) +
    ggplot2::geom_boxplot()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$fill, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, fill = "test fill")
  expect_equal(p@labels$fill, "test fill")
})

test_that("label linetype", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$linetype, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, linetype = "test linetype")
  expect_equal(p@labels$linetype, "test linetype")
})

test_that("col, color, and colour are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p1 <- p0 + ys_gg_labs(spec, colour = "test colour")
  p2 <- p0 + ys_gg_labs(spec, col = "test colour")
  p3 <- p0 + ys_gg_labs(spec, color = "test colour")
  expect_equal(p1@labels$colour, p2@labels$colour)
  expect_equal(p1@labels$colour, p3@labels$colour)
})

test_that("lty and linetype are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p1 <- p0 + ys_gg_labs(spec, linetype = "test linetype")
  p2 <- p0 + ys_gg_labs(spec, lty = "test linetype")
  expect_equal(p1@labels$linetype, p2@labels$linetype)
})

test_that("label shape", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, shape = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(p@labels$shape, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, shape = "test shape")
  expect_equal(p@labels$shape, "test shape")
})
