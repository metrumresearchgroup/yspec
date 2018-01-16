library(yspec)
library(testthat)

test_that("error is generated if spec file doesn't exist", {
  expect_error(load_spec("hey.yml"))
})

test_that("error if yaml code is bad", {
  expect_error(load_spec_ex("error.yml"),
               regexp = "failed to parse error.yml")
  expect_error(load_spec_ex("error.yml"),
               regexp = "Duplicate map key")
})


