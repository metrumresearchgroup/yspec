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

test_that("ys_document_namespace is invoked on render define", {
  spec <- ys_help$spec()
  temp <- tempdir()
  x <- ys_document(
    spec, 
    run_pandoc = FALSE, 
    build_dir = temp, 
    quiet = TRUE
  )
  lines <- readLines(file.path(temp,x))
  lines <- lines[grepl("LDOS", lines)]
  expect_match(lines, "(milligrams)", fixed = TRUE)
  expect_equal(spec$LDOS$unit, "mg")
})

test_that("working document header is data_stem.ext", {
  spec <- ys_help$spec()
  ds <- pull_meta(spec, "data_stem")
  file <- ys_document(
    spec, 
    type = "working", 
    output_format = "md_document", 
    quiet = TRUE
  )
  doc <- readLines(file)
  expect_match(doc[1], paste0(ds, ".csv"))

  spec <- yspec:::push_meta(spec, "data_stem", "new-data-stem")
  file <- ys_document(
    spec, 
    type = "working", 
    output_format = "md_document", 
    quiet = TRUE
  )
  doc <- readLines(file)
  expect_match(doc[1], "# new-data-stem.csv", fixed = TRUE)
})

test_that("change extension for working document header", {
  spec <- ys_help$spec()
  spec <- yspec:::push_meta(spec, "data_stem", "new-data-stem")
  file <- ys_document(
    spec, 
    type = "working", 
    output_format = "md_document", 
    quiet = TRUE, 
    ext = ".bar"
  )
  doc <- readLines(file)
  expect_match(doc[1], "# new-data-stem.bar", fixed = TRUE)
})
