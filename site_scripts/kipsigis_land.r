
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/kipsigis_land.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"land"]) | is.na(data[,"landf"]) |
  is.na(data[,"age"]) | is.na(data[,"agef"]) | is.na(data[,"male"]))
data <- data[-drop,]
expect_true(nrow(data) == 270)

## Create Variables and Recreate 2009 Results

land <- data[,"land"]
landf <- data[,"landf"]
age <- data[,"age"]
age2 <- age^2
male <- data[,"male"]
female <- 1-male
hhid <- data[,"fid"]

# Stata analysis used "errors in variables regression", which I can't replicate in R
# notably they didn't cluster on hhid...I'm guessing because they couldn't
m0 <- lm(land ~ landf + age + age2 + female)
display.er(m0, cluster=hhid)
display(m0, digits=3)
robust.se(m0, hhid)
# ** Published Beta given here:  0.357 (0.041)
# had they just done the standard elasticity it would be 0.330 (0.030)
# the non-elastic value is 0.036 (0.003)
