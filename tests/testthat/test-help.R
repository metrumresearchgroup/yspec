library(yspec)

context("test-help.R")

test_that("assets are exported [YSP-TEST-0048]", {
  where <- file.path(tempdir(),"assets")
  x <- ys_help$export(where, overwrite = TRUE)
  expect_true(dir.exists(where))
  fi <- list.files(where)
  chk <- c("ys_example.Rmd", "ysdb_internal.yml","analysis1.yml", 
           "analysis1.csv", "ys_get_assets.md")
  expect_true(identical(sort(fi),sort(chk)))
})

test_that("help object printing [YSP-TEST-0049]", {
  x <- capture.output(print(ys_help)) 
  expect_true(any(grepl("spec()", x, fixed = TRUE)))
  expect_true(any(grepl("file()", x, fixed = TRUE)))
  expect_true(any(grepl("data()", x, fixed = TRUE)))
  expect_true(any(grepl("csv()", x, fixed = TRUE)))
  expect_true(any(grepl("proj()", x, fixed = TRUE)))
  expect_true(any(grepl("yaml()", x, fixed = TRUE)))
  expect_true(any(grepl("ref()", x, fixed = TRUE)))
  expect_true(any(grepl("db()", x, fixed = TRUE)))
  expect_true(any(grepl("example()", x, fixed = TRUE)))
  expect_true(any(grepl("export(", x, fixed = TRUE)))
})

test_that("help object usage [YSP-TEST-0050]", {
  x <- ys_help$spec()
  expect_is(x,"yspec [YSP-TEST-0086]")
  x <- ys_help$csv()
  expect_equal(basename(x), "analysis1.csv")
  x <- ys_help$file()
  expect_equal(basename(x), "analysis1.yml")
  x <- ys_help$data()
  expect_is(x,"data.frame")
  x <- ys_help$proj()
  expect_is(x, "yproj")
})

