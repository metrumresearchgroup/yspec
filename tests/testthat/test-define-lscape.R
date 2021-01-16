library(yspec)
library(testthat)

context("test-define-lscape")

test_that("test-define-lscape create data frame for lscape table", {
  spec <- ys_help$spec()
  df <- ys_df_wide(spec)
  expect_is(df, "data.frame")
  expect_equal(
    names(df), 
    c("Variable", "Label", "Unit", "Type", "Decode/Range/Comment")
  )
  df2 <- lscape_table(spec, title_case = TRUE)
  expect_identical(df2$Label, tools::toTitleCase(df$Label))
})
