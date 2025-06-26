library(yspec)
library(testthat)

# .test_load <- yspec:::.test_load
# .test_spec <- yspec:::.test_spec

context("test-fda_define")

test_that("fda_define [YSP-TEST-0039]", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))
  pr <- ys_project(sp)
  met <- get_meta(pr)
  tex <- fda_define(met[["spec_file"]])
  expect_is(tex,"character")
})

test_that("data_stem is respected in regulatory define [YSP-TEST-0040]", {
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

test_that("skip printing unit in define.pdf when blank", {
  spec <- yspec:::test_spec_test("no-unit.yaml")
  proj <- ys_project(spec)
  m <- get_meta(proj)
  ans <- fda_define(m$spec_file)
  ans <- ans[grepl("^[A,B,C] \\&", ans)]
  expect_no_match(ans[1], "unit")
  expect_match(ans[2], "unit: ng/mL", fixed = TRUE)
  expect_no_match(ans[3], "unit")
})
