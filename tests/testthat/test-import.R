library(yspec)
library(purrr)
library(testthat)

context("test-import")

loc1 <- system.file("spec","import-example.yml",package="yspec")
loc2 <- system.file("spec","DEM104101F_PKPD.yml",package="yspec")
loc3 <- system.file("spec","import-example-add.yml",package="yspec")

test_that("import - identical spec", {
  a <- ys_load(loc1)
  b <- ys_load(loc2)
  expect_identical(a,b)
})

test_that("import - identical spec with additional column", {
  a <- ys_load(loc1)
  b <- ys_load(loc3)
  expect_identical(
    c(names(a),"ADD"),
    names(b)
  )
})

