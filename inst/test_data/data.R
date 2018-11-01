
library(dplyr)
library(purrr)
library(yaml)
library(yspec)


set.seed(1100)

n <- 50
data <- data_frame(
  TIME = seq(n),
  WT = runif(n, 20, 120),
  DV = runif(n, 0, 1000),
  SEX = rbinom(n, 1,0.5),
  STUDY = sample(letters[1:3], size = n, replace = TRUE)
)

data2 <- data
data2[] <- imap(data, .f = function(x,y) {
  if(y=="TIME") return(x)
  i <- sample(seq(length(x)), size = 5)
  x[i] <- NA
  x
})


time <- list(type = "numeric", range = c(1,50), short = "time", unit = "hr")
wt <- list(range = c(10,150), short = "weight", unit = "kg")
dv <- list(range = c(0,10000), long  = "dependent variable", unit = "ng/ml")
study <- list(type = "character", short = "study", values = letters[1:3])
sex <- list(short = "female sex", values = c(0,1), decode = c("male", "female"))
spec <- list(TIME = time, WT = wt, DV = dv, SEX = sex, STUDY = study)

file <- "inst/test_data/spec.yml"
writeLines(as.yaml(spec),con = file)
saveRDS(file = "inst/test_data/test1.RDS", data)
saveRDS(file = "inst/test_data/test2.RDS", data2)


data <- readRDS("inst/test_data/test1.RDS")
data2 <- readRDS("inst/test_data/test2.RDS")
spec <- yspec::load_spec(file)
check_data(data2, spec)






