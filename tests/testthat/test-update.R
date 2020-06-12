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

test_that("update short column", {
  sp <- ys_help$spec()  
  sp2 <- update_short(sp, ID = "subject", AMT = "dose")
  expect_true(sp2$ID$short != sp$ID$short)
  expect_true(sp2$AMT$short != sp$AMT$short)
  expect_identical(sp2$ID$short, "subject")
  expect_identical(sp2$AMT$short, "dose")
  sp3 <- update_short(sp)
  expect_identical(sp,sp3)
  expect_error(update_short(sp, kyle = "baron", AMT = "dose"))
})

