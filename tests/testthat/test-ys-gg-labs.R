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
  expect_equal(ggplot2::get_labs(p0)$x, "TIME")
  expect_equal(ggplot2::get_labs(p0)$y, "DV")

  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("labs overrides spec for x- and y-", {
  p <- p0 + ys_gg_labs(spec, labs)
  expect_equal(ggplot2::get_labs(p)$x, "Time after first dose")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("force in x- or y-", {
  p <- p0 + ys_gg_labs(spec, labs, x = "A", y = "B")
  expect_equal(ggplot2::get_labs(p)$x, "A")
  expect_equal(ggplot2::get_labs(p)$y, "B")
})

test_that("transformed x- or y- is ignored", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Time (hour)")
  expect_equal(ggplot2::get_labs(p)$y, "log(DV)")
})

test_that("transformed x- or y- in labs", {
  labs[["log(DV)"]] <- "log-transformed DV"
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, log(DV))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec, labs)
  expect_equal(ggplot2::get_labs(p)$x, "Time after first dose")
  expect_equal(ggplot2::get_labs(p)$y, "log-transformed DV")
})

test_that("factor x- or y- is passed through", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(factor(CP), DV)) +
    ggplot2::geom_boxplot()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$x, "Child-Pugh score")
  expect_equal(ggplot2::get_labs(p)$y, "Concentration (ng/mL)")
})

test_that("label colour", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$colour, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, colour = "test colour")
  expect_equal(ggplot2::get_labs(p)$colour, "test colour")
})

test_that("label fill", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, fill = factor(CP))) +
    ggplot2::geom_boxplot()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$fill, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, fill = "test fill")
  expect_equal(ggplot2::get_labs(p)$fill, "test fill")
})

test_that("label linetype", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$linetype, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, linetype = "test linetype")
  expect_equal(ggplot2::get_labs(p)$linetype, "test linetype")
})

test_that("col, color, and colour are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, colour = factor(CP))) +
    ggplot2::geom_point()
  p1 <- p0 + ys_gg_labs(spec, colour = "test colour")
  p2 <- p0 + ys_gg_labs(spec, col = "test colour")
  p3 <- p0 + ys_gg_labs(spec, color = "test colour")
  expect_equal(ggplot2::get_labs(p1)$colour, ggplot2::get_labs(p2)$colour)
  expect_equal(ggplot2::get_labs(p1)$colour, ggplot2::get_labs(p3)$colour)
})

test_that("lty and linetype are equivalent", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, linetype = factor(CP))) +
    ggplot2::geom_line()
  p1 <- p0 + ys_gg_labs(spec, linetype = "test linetype")
  p2 <- p0 + ys_gg_labs(spec, lty = "test linetype")
  expect_equal(ggplot2::get_labs(p1)$linetype, ggplot2::get_labs(p2)$linetype)
})

test_that("label shape", {
  p0 <-
    ggplot2::ggplot(data, ggplot2::aes(TIME, DV, shape = factor(CP))) +
    ggplot2::geom_point()
  p <- p0 + ys_gg_labs(spec)
  expect_equal(ggplot2::get_labs(p)$shape, "Child-Pugh score")
  p <- p0 + ys_gg_labs(spec, shape = "test shape")
  expect_equal(ggplot2::get_labs(p)$shape, "test shape")
})
