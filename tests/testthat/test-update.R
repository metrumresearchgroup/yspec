library(yspec)
library(purrr)

context("test-update.R")

pr <- ys_help$proj()

test_that("update projectnumber", {
  proj <- update(pr, projectnumber = "9999")
  expect_equal(get_meta(proj)[["projectnumber"]],"9999")
})

test_that("update sponsor", {
  proj <- update(pr, sponsor = "new sponsor name")
  expect_equal(get_meta(proj)[["sponsor"]],"new sponsor name")
})
