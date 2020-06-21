
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/kipsigis_livestock.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"livestock"]) | is.na(data[,"livestockf"]) |
  is.na(data[,"age"]) | is.na(data[,"agef"]) | is.na(data[,"male"]))
data <- data[-drop,]
expect_true(nrow(data) == 270)
# 2009 paper used 270 individuals


## Create Variables and Recreate 2009 Results

livestock <- data[,"livestock"]
livestockf <- data[,"livestockf"]
age <- data[,"age"]
age2 <- age^2
male <- data[,"male"]
female <- 1-male
hhid <- data[,"fid"]

# Stata analysis used "errors in variables regression", which I can't replicate in R
# notably he didn't cluster on hhid...I'm guessing because they couldn't

m0 <- lm(livestock ~ livestockf + age + age2 + female)
display(m0, digits=3)
robust.se(m0, hhid)

# ** Published beta given here: 0.635 (0.098)
# had they just done the standard elasticity it would be 0.468 (0.098)
# the non-elastic value is 0.076 (0.018)
