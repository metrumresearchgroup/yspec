
library(yspec)
library(testthat)
library(dplyr)

context("test-check")

data_loc <- system.file("test_data", package = "yspec")
data1 <- readRDS(file.path(data_loc, "test1.RDS"))
data2 <- readRDS(file.path(data_loc, "test2.RDS"))
spec <- load_spec(file.path(data_loc, "spec.yml"))

test_that("check with no missing values [YSP-TEST-0009]", {
  expect_message(check_data(data1, spec),  
                 regexp = "The data set passed all checks.")
})

test_that("check with missing values [YSP-TEST-0010]", {
  expect_message(check_data(data2, spec),  
                 regexp = "The data set passed all checks.")
})

test_that("missing column [YSP-TEST-0011]", {
  data <- dplyr::select(data1, -WT, -STUDY)
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
})

test_that("extra column [YSP-TEST-0012]", {
  data <- mutate(data1, FOOO = 1)
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
})

test_that("column with wrong name [YSP-TEST-0013]", {
  data <- dplyr::rename(data1, WEIGHT = WT)
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
})

test_that("column with wrong name [YSP-TEST-0013]", {
  names(spec) <- paste0(names(spec),names(spec))
  expect_error(check_data(data1, spec), 
               regexp = "Please review messages and re-check")
})

test_that("extra column [YSP-TEST-0012]", {
  data <- mutate(data1, FOOO = 1)
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
})

test_that("cols not sorted issue-54 [YSP-TEST-0014]", {
  data <- dplyr::select(data1, sort(names(data1)))
  expect_error(ys_check(data,spec), 
               regexp = "Please review messages and re-check")
})

test_that("continuous out of range [YSP-TEST-0015]", {
  data <- mutate(data1, WT = 1E6)
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
})

test_that("categorical out of range [YSP-TEST-0016]", {
  data <- mutate(data1, STUDY = "A")
  expect_error(check_data(data, spec), 
               regexp = "Please review messages and re-check")
  expect_message(try(check_data(data, spec), silent=TRUE), 
                 regexp = "discrete value out of range: STUDY")
})

test_that("all NA returns success [YSP-TEST-0017]", {
  data <- mutate(data1, WT = as.numeric(NA))
  expect_message(check_data(data, spec),  
                 regexp = "The data set passed all checks.")
  
  data <- mutate(data1, SEX = as.character(NA))
  expect_message(check_data(data,spec), 
                 regexp = "The data set passed all checks.")
  
  data$SEX[3] <- 4
  expect_error(check_data(data,spec))
  
  data$SEX[3] <- 1
  expect_message(check_data(data,spec),
                 regexp = "The data set passed all checks.")
})

test_that("no error on fail [YSP-TEST-0018]", {
  data <- dplyr::select(data1, -WT, -STUDY)
  expect_false(ys_check(data, spec, error_on_fail = FALSE))
  expect_message(ys_check(data,spec,error_on_fail = FALSE), 
                 regex = "names in spec but not in data")
})

test_that("ys_check return value [YSP-TEST-0019]", {
  good <- data1
  bad <- dplyr::select(good, -WT, -STUDY)
  expect_true(ys_check(good,spec))
  expect_true(ys_check(good,spec,error_on_fail = FALSE))
  expect_false(ys_check(bad,spec,error_on_fail = FALSE))
  expect_error(ys_check(bad,spec))
})

