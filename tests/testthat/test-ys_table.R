
library(yspec)
library(testthat)

context("test-define")

test_that("ys_table returns report table code [YSP-TEST-0127]", {
  spec <- ys_help$spec()
  tex <- ys_table(spec)
  expect_is(tex, "character")
  expect_match(tex[3], "VARIABLE")
  expect_match(tex, "Continued on next page", all = FALSE)
  tex <- ys_table(spec, widths_ = c(1.234,999,44,0))
  expect_match(tex, "1.234", fixed = TRUE, all = FALSE)
  expect_match(tex, "999", fixed = TRUE, all = FALSE)
})

test_that("ns argument sets namespace prior to generating table", {
  spec <- ys_help$spec()
  
  # default: ns = "tex", tex = TRUE -> tex namespace applied
  # LDOS has no unit.tex, so base unit "mg" is used
  # DV has unit.tex = "$\mu$g/L"
  tex <- ys_table(spec)
  expect_match(tex[grepl("LDOS", tex)], "unit: mg", fixed = TRUE)
  expect_match(tex[grepl("dependent", tex)], "unit: $\\mu$g", fixed = TRUE)
  
  # ns = "define": unique(c("tex", "define")) -> both applied
  # LDOS has unit.define = "milligrams"; DV still gets unit.tex
  tex <- ys_table(spec, ns = "define")
  expect_match(tex[grepl("LDOS", tex)], "unit: milligram", fixed = TRUE)
  expect_match(tex[grepl("dependent", tex)], "unit: $\\mu$g", fixed = TRUE)
  
  # ns = NULL: no namespace applied, base values used
  # DV base unit is "micrograms/L"
  tex <- ys_table(spec, ns = NULL)
  expect_match(tex[grepl("LDOS", tex)], "unit: mg", fixed = TRUE)
  expect_match(tex[grepl("dependent", tex)], "unit: micrograms/L", fixed = TRUE)
})
