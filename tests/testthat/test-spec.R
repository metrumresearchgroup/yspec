library(yspec)

context("text-spec")

test_that("yspec", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))  
  expect_is(as.list(sp),"list")
  txt <- capture.output(sp)
  expect_true(grepl(" *ID", txt[4]))
  expect_is(sp$WT, "ycol")
  expect_is(sp[["WT"]],"ycol")
  expect_is(as.data.frame(sp), "data.frame")
  expect_is(head(sp), "data.frame")
  expect_is(tail(sp), "data.frame")
  expect_is(get_meta(sp),"list")
  
  sp <- load_spec_ex()
  expect_equal(yspec:::unit(sp$AMT), "mg")
  expect_equal(yspec:::type(sp$STUDY), "character")
  expect_equal(yspec:::short(sp$CLCR), "creatinine clearance")
  expect_equal(yspec:::Range(sp$EGFR), "10 to 300")
  expect_equal(primary_keys(sp), c("ID", "EVID", "SEQ"))
  txt <- capture.output(summary(sp))
  expect_true(grepl("EGFR", txt[13]))
})

