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
