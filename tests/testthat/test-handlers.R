library(yspec)

context("test-handlers")

test_that("handle value:decode [YSP-TEST-0046]", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))  
  expect_is(sp, "yspec")
})

test_that("decode_values [YSP-TEST-0047]", {
  sp <- load_spec_ex("decode_values.yml")  
  expect_is(sp, "yspec")
  sp <- as.list(sp)
  expect_equivalent(sp[["example1"]][["values"]],sp[["example2"]][["values"]])
  expect_equivalent(sp[["example1"]][["values"]],sp[["example3"]][["values"]])
  expect_equivalent(sp[["example1"]][["values"]],sp[["example4"]][["values"]])
})

