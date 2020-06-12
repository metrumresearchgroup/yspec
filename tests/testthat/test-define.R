library(yspec)
library(testthat)
library(purrr)
library(pander)

context("test-define")

test_that("define", {
  outdir <- normalizePath(tempdir())
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  expect_is(sp, "yspec")

  pr <- ys_project(sp)
  
  skip()
  out <- ys_document(sp, type = "working",quiet=TRUE,
                     output_dir = outdir)
  out <- ys_document(pr, type = "regulatory", build_dir = mrgtemplate(),
                     quiet=TRUE, output_dir = outdir)

})

test_that("md_outline", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  expect_is(sp, "yspec")
  pr <- ys_project(sp)
  expect_is(pr,"yproj")
  yamlfile <- get_meta(pr)[["spec_file"]]
  skip()
  define_for_rmd(yamlfile,"md_outline")
})

test_that("pander_table", {
  pr <- ys_project_file(file_spec_ex("DEM104101F_PK.yml"))
  expect_is(pr,"yproj")
  yamlfile <- ys_spec_file(pr)
  define_for_rmd(yamlfile,"pander_table")
})
