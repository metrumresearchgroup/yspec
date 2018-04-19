library(yspec)
library(testthat)

context("test-axis_labs")

spec <- load_spec_ex()

test_that("axis labels are generated", {
  labs <- axis_labs(spec, WT, EGFR, ALB)  
  expect_identical(names(labs), c("WT", "EGFR", "ALB"))
  expect_identical(unname(labs), c("Weight (kg)", "eGFR (ml/min/1.73m2)", 
                                   "Albumin (g/dL)"))
})

test_that("axis col-labels are generated", {
  labs <- axis_col_labs(spec, WT, ALB)  
  expect_identical(names(labs), c("WT","ALB"))
  expect_identical(unname(labs), c("WT//Weight (kg)", "ALB//Albumin (g/dL)"))
})

test_that("take axis info as-is", {
  labs <- axis_labs(spec, WT, ALB, .fun = axis_asis)  
  expect_identical(labs, c(WT = "Weight", ALB = "Albumin"))
})



