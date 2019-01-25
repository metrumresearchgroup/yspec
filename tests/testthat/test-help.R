library(yspec)
library(purrr)

context("test-help.R")

test_that("assets are exported", {
  where <- file.path(tempdir(),"assets")
  x <- ys_get_assets(where, overwrite = TRUE)
  expect_true(dir.exists(where))
  fi <- list.files(where)
  chk <- c("ys_example.Rmd", "ysdb_internal.yml", 
           "analysis1.yml", "analysis1.csv", "ys_get_assets.md")
  expect(identical(sort(fi),sort(chk)))

})

