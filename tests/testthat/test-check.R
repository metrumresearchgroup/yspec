
library(yspec)
library(testthat)
library(dplyr)

data_loc <- system.file("test_data", package = "yspec")
data1 <- readRDS(file.path(data_loc, "test1.RDS"))
data2 <- readRDS(file.path(data_loc, "test2.RDS"))
spec <- load_spec(file.path(data_loc, "spec.yml"))

expect_works <- function(...) {
  expect_message(...,  regexp = "Everything checks out!")
}

test_that("check with no missing values", {
  expect_works(check_data(data1, spec))
})

test_that("check with missing values", {
  expect_works(check_data(data2, spec))
})

test_that("missing column", {
  data <- dplyr::select(data1, -WT)
  expect_error(check_data(data, spec))
})

test_that("extra column", {
  data <- mutate(data1, FOOO = 1)
  expect_error(check_data(data, spec))
})

test_that("column with wrong name", {
  data <- dplyr::rename(data1, WEIGHT = WT)
  expect_error(check_data(data, spec))
})

test_that("extra column", {
  data <- mutate(data1, FOOO = 1)
  expect_error(check_data(data, spec))
})

test_that("continuous out of range", {
  data <- mutate(data1, WT = 1E6)
  expect_error(check_data(data, spec))
})

test_that("categorical out of range", {
  data <- mutate(data1, STUDY = "A")
  expect_error(check_data(data, spec))
})

test_that("all NA returns success", {
  data <- mutate(data1, WT = as.numeric(NA))
  expect_works(check_data(data, spec))
})

