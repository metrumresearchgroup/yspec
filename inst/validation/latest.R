library(yaml)

x <- yaml.load_file("inst/validation/yspec-stories.yaml")

stories <- names(x)
stories <- sub("YSP-S", "", stories)
stories <- as.integer(stories)

tests <- unlist(lapply(x,  function(x) x$tests), use.names=FALSE)
dupt <- any(duplicated(tests))
tests <- sub("YSP-TEST-", "", tests)
tests <- as.integer(tests)

message("last story: ", max(stories))
message("last test: ", max(tests))
