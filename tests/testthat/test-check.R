
library(yspec)
library(testthat)
library(dplyr)

data_loc <- system.file("test_data", package = "yspec")
data1 <- readRDS(file.path(data_loc, "test1.RDS"))
data2 <- readRDS(file.path(data_loc, "test2.RDS"))
spec <- load_spec(file.path(data_loc, "spec.yml"))

test_that("check with no missing values", {
  expect_message(check_data(data1, spec),  
                 regexp = "The data set passed all checks.")
})

test_that("check with missing values", {
  expect_message(check_data(data2, spec),  
                 regexp = "The data set passed all checks.")
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

