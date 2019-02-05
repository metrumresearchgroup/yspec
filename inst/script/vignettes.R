library(purrr)
library(rmarkdown)

files <- list.files("vignettes", pattern = "\\.Rmd$", full.names=TRUE)
map(files, render, output_dir = "inst/doc", output_options = list(keep_md = FALSE))
file.rename("inst/doc/_index.html", "inst/doc/index.html")
