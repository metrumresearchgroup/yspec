library(yspec)
library(testthat)

# .test_load <- yspec:::.test_load
# .test_spec <- yspec:::.test_spec

context("test-fda_define")

test_that("fda_define", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  met <- get_meta(pr)
  tex <- fda_define(met[["spec_file"]])
  expect_is(tex,"character")
})

test_that("data_stem is respected in regulatory define", {
  spec <- yspec:::test_spec_test("issue-35.yml")
  proj <- ys_project(spec)
  m <- get_meta(proj)
  ans <- fda_define(m$spec_file)
  pr <- proj[[1]]
  res <- regexpr(pr$data_stem, ans, fixed = TRUE)
  expect_true(sum(res > 0)==2)
  res <- regexpr(basename(pr$xpt_file), ans, fixed = TRUE)
  expect_true(sum(res > 0) ==2)
  expect_equal(basename(pr$xpt_file),paste0(pr$data_stem,".xpt"))
})


