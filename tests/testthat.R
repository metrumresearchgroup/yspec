
Sys.setenv("R_TESTS" = "")
library(testthat)
library(yspec)
test_check("yspec", reporter="summary")

