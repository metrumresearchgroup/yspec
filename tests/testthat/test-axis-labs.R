library(yspec)
library(testthat)

context("test-axis_labs")

spec <- load_spec_ex()

test_that("axis labels are generated [YSP-TEST-0001]", {
  labs <- axis_labs(spec, "WT,EGFR,ALB")  
  expect_identical(names(labs), c("WT", "EGFR", "ALB"))
  expect_identical(unname(labs), c("Weight (kg)", "eGFR (ml/min/1.73m2)", 
                                   "Albumin (g/dL)"))
})

test_that("axis col-labels are generated [YSP-TEST-0002]", {
  labs <- axis_col_labs(spec, dplyr::vars(WT, ALB))  
  expect_identical(names(labs), c("WT","ALB"))
  expect_identical(unname(labs), c("WT//Weight (kg)", "ALB//Albumin (g/dL)"))
})


test_that("axis col-labels title case [YSP-TEST-0003]", {
  labs <- axis_col_labs(spec, dplyr::vars(WT, ALB), title_case = TRUE)  
  expect_identical(names(labs), c("WT","ALB"))
  expect_identical(unname(labs), c("WT//Weight (kg)", "ALB//Albumin (g/dL)"))
})

test_that("axis col-labels short max [YSP-TEST-0004]", {
  labs <- axis_col_labs(spec, dplyr::vars(WT, ALB), short_max = 4)  
  expect_identical(names(labs), c("WT","ALB"))
  expect_identical(unname(labs), c("WT//WT (kg)", "ALB//Albumin (g/dL)"))
})



test_that("take axis info as-is [YSP-TEST-0005]", {
  labs <- axis_labs(spec, dplyr::vars(WT, ALB), .fun = axis_asis)  
  expect_identical(labs, c(WT = "Weight", ALB = "Albumin"))
})

test_that("axis lab is in title case [YSP-TEST-0006]", {
  labs <- axis_labs(spec,"ALB,WT", title_case = TRUE)  
  labs <- strsplit(labs," +")
  expect_equal(labs$ALB[1], "Albumin")
  expect_equal(labs$WT[1], "Weight")
})

test_that("axis lab has limited length [YSP-TEST-0007]", {
  spec <- ys_help$spec()
  labs <- axis_labs(spec,"HT,WT", short_max = 2)  
  labs <- strsplit(labs, " +")
  expect_equal(labs$HT[1], "HT")
  expect_equal(labs$WT[1], "WT")
})
