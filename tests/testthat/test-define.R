library(yspec)
library(testthat)
library(purrr)

context("test-define.R")

test_that("define", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  expect_is(sp, "yspec")
  out <- ys_document(sp, type = "working",quiet=TRUE)
  pr <- ys_project(sp)
  out <- ys_document(pr, type = "regulatory", build_dir = mrgtemplate(),quiet=TRUE)
  
})

test_that("md_outline", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  expect_is(sp, "yspec")
  pr <- ys_project(sp)
  expect_is(pr,"yproj")
  yamlfile <- get_meta(pr)[["spec_file"]]
  define_for_rmd(yamlfile,"md_outline")
})

test_that("pander_table", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  expect_is(pr,"yproj")
  yamlfile <- get_meta(pr)[["spec_file"]]
  define_for_rmd(yamlfile,"pander_table")
})