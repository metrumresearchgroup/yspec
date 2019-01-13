library(yspec)
library(testthat)
library(purrr)
.test_load <- yspec:::.test_load
.test_spec <- yspec:::.test_spec


test_that("fda_define", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  met <- get_meta(pr)
  tex <- fda_define(met[["spec_file"]])
  expect_is(tex,"character")
})



