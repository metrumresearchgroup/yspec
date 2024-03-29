library(yspec)
library(testthat)

context("test-define")

test_that("define [YSP-TEST-0031]", {
  
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  expect_is(sp, "yspec")
  pr <- ys_project(sp)
  
  out <- suppressWarnings(
    ys_document(sp, type = "working", quiet = TRUE, output_dir = tempdir())
  )
  
  expect_is(out,"character")
  
  out <- ys_document(pr, type = "regulatory", build_dir = tempdir(),
                     quiet=TRUE, output_dir = tempdir())
  
  expect_is(out,"character")
  
})

# test_that("md_outline", {
#   sp <- load_spec_ex(("DEM104101F_PK.yml"))
#   expect_is(sp, "yspec [YSP-TEST-0086]")
#   pr <- ys_project(sp)
#   expect_is(pr,"yproj")
#   yamlfile <- pull_meta(pr,"spec_file")
#   ans <- define_for_rmd(yamlfile,"md_outline")
#   expect_is(ans,"character")
# })
# 
# test_that("pander_table", {
#   pr <- ys_project_file(file_spec_ex("DEM104101F_PK.yml"))
#   expect_is(pr,"yproj")
#   yamlfile <- ys_spec_file(pr)
#   ans <- define_for_rmd(yamlfile,"pander_table")
#   expect_is(ans,"character")
# })
