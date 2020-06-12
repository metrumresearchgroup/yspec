library(yspec)

context("test-update.R")

pr <- ys_help$proj()

test_that("update projectnumber - yproj", {
  proj <- update(pr, projectnumber = "9999")
  expect_equal(get_meta(proj)[["projectnumber"]],"9999")
})

test_that("update sponsor - yproj", {
  proj <- update(pr, sponsor = "new sponsor name")
  expect_equal(get_meta(proj)[["sponsor"]],"new sponsor name")
})

sp <- ys_help$spec()

test_that("update projectnumber - yspec", {
  proj <- update(sp, projectnumber = "9999")
  expect_equal(get_meta(proj)[["projectnumber"]],"9999")
})

test_that("update sponsor - yspec", {
  proj <- update(sp, sponsor = "new sponsor name")
  expect_equal(get_meta(proj)[["sponsor"]],"new sponsor name")
})





