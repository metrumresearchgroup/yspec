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

test_that("error if null values", {
  l <- list(NAME  = list(values = list(A=NULL, B = NULL)))
  expect_error(test_spec_list(l),label="values field includes NULLs")
})

test_that("decodes, named list", {
  l <- list(NAME  = list(values = list(A=1,B=2), type = "character"))
  x <- test_spec_list(l)
 expect_is(x, "yspec")
 expect_equal(x$NAME$decode, c("A", "B"))
 expect_equal(x$NAME$values, c(A=1,B=2))
 expect_equal(x$NAME$type,"character")
})


x <- yaml::yaml.load("
NAME:
  values: 
    2: a
    AWWE: b
")


x <- yaml::yaml.load("
NAME:
  values: [a,b,c]
  valuz: {a, b, c}
  valuzx: {a: 1, b: 3, c: 4}
")



yaml::yaml.load("
? - Detroit Tigers
  - Chicago cubs
                :
                - 2001-07-23
                
                ? [ New York Yankees,
                Atlanta Braves ]
                : [ 2001-07-02, 2001-08-12,
                2001-08-14 ]
")



