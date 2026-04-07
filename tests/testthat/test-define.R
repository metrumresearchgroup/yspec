library(yspec)
library(testthat)

context("test-define")

test_that("define [YSP-TEST-0031]", {

  spec <- ys_help$spec()
  expect_is(spec, "yspec")

  out <- ys_document(spec, type = "working", quiet = TRUE,
                     output_dir = tempdir(), build_dir = tempdir())

  expect_is(out,"character")

  out <- ys_document(spec, type = "regulatory", build_dir = tempdir(),
                     quiet=TRUE, output_dir = tempdir())

  expect_is(out,"character")

})

test_that("ns argument applies namespace before rendering document", {
  spec <- ys_help$spec()
  # default ns = "tex": LDOS has no unit.tex, so base unit "mg" is used
  x <- ys_document(spec, run_pandoc = FALSE, output_dir = tempdir(),
                   build_dir = tempdir(), quiet = TRUE)
  output <- readLines(file.path(tempdir(), basename(x)))
  expect_match(output[grepl("LDOS", output)], "mg", fixed = TRUE)
  # ns = "define": define namespace applied, LDOS unit.define = "milligrams"
  x <- ys_document(spec, ns = "define", run_pandoc = FALSE,
                   output_dir = tempdir(), build_dir = tempdir(), quiet = TRUE)
  output <- readLines(file.path(tempdir(), basename(x)))
  expect_match(output[grepl("LDOS", output)], "milligram", fixed = TRUE)
  # ns = "error": not a valid namespace, raises a warning
  expect_warning(
    ys_document(spec, ns = "error", run_pandoc = FALSE, output_dir = tempdir(),
                build_dir = tempdir(), quiet = TRUE),
    "not a namespace"
  )
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
