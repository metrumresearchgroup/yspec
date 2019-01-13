library(yspec)
library(purrr)


test_that("handle value:decode", {
  sp <- load_spec_ex(("DEM104101F_PK.yml"))  
  expect_is(sp, "yspec")
})

