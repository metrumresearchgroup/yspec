library(yspec)
library(testthat)
library(purrr)

test_that("define", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  out <- ys_document(sp, type = "working")
  pr <- ys_project(sp)
  out <- ys_document(pr, type = "regulatory", build_dir = mrgtemplate())
})

test_that("md_outline", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  yamlfile <- get_meta(pr)[["proj_file"]]
  define_for_rmd(yamlfile,"md_outline")
})

test_that("pander_table", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  yamlfile <- get_meta(pr)[["proj_file"]]
  define_for_rmd(yamlfile,"pander_table")
})