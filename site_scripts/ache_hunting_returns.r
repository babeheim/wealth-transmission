
rm(list = ls())

source("./project_support.r")

## Create Variables and Recreate 2009 Results

data <- read.csv("./data/ache_hunting_returns.csv", stringsAsFactors = FALSE)

expect_true(nrow(data) == 49)

m0 <- lm(rr ~ rrf, data = data)

display.er(m0)
# ** Publish Beta here:  0.081 (0.273)
# They used *errors in variables regression*, which I can't replicate in R,
# so the estimates are a bit different.  Right ballpark though.
