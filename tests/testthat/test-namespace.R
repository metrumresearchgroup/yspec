library(yspec)
library(testthat)
library(dplyr)

context("test-namespace")
works <- system.file("spec", "test", "namespace-works.yaml", package = "yspec")


test_that("load a yaml file and parse namespaces", {
  spec <- ys_load(works)
  expect_message(ys_namespace(spec), msg = "namespaces:")
  ns <- pull_meta(spec, "namespace")
  expect_equal(ns, c("base", "plot", "tex"))
})

test_that("switch to tex namespace", {
  spec <- ys_load(works)
  a <- spec
  b <- ys_namespace(a, "tex")
  expect_equal(a$WT$unit, b$WT$unit)
  expect_equal(a$DV$unit, "micrograms / ml")
  expect_equal(b$DV$unit, "$\\\\mu$g / ml")
  expect_error(ys_namespace(a, "kyle"))
  
  c <- yspec:::try_tex_namespace(a)
  expect_identical(c, b)
})

test_that("revert to base namespace", {
  spec <- ys_load(works)
  a <- spec
  b <- ys_namespace(a, "tex")
  c <- ys_namespace(b, "base")
  expect_equal(a,c)
})

test_that("alternate decode in namespace", {
  a <- ys_load(works)
  b <- ys_namespace(a, "plot")
  expect_equal(a$SEX$decode, c("m", "f"))
  expect_equal(b$SEX$decode, c("male", "female"))
  file <- system.file(
    "spec", "test", "namespace-error-decode.yaml", 
    package = "yspec"
  )
  expect_error(ys_load(file), msg = "decode is not the correct length")
})

test_that("multiple namespaces are handled properly", {
  x <- yspec:::test_spec_test("namespace-multiple.yml")
  #plot <- ys_namespace(x, "plot")
  #expect_identical(x$STUDY$short, "short base")
  #expect_identical(plot$STUDY$short, "short plot")
  #lc <- ys_namespace(x, "lc")
  #uc <- ys_namespace(x, "uc")
  #expect_identical(x$STUDY$decode, c("Phase 1", "Phase 2", "Phase 3"))
  #expect_identical(plot$STUDY$decode, c("Phase 1", "Phase 2", "Phase 3"))
  #expect_identical(lc$STUDY$decode, c("phase 1", "phase 2", "phase 3"))
  #expect_identical(uc$STUDY$decode, c("PHASE 1", "PHASE 2", "PHASE 3"))
})
