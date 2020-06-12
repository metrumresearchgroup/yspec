library(yspec)

context("test-ycol")

test_that("ycol", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))  
  txt <- capture.output(sp$WT)
  expect_true(grepl("WT", txt[2]))
  expect_true(grepl("numeric", txt[3]))
  txt <- capture.output(sp$SEX)
  expect_true(grepl("SEX", txt[2]))
  expect_is(as.list(sp$WT), "list")
})


