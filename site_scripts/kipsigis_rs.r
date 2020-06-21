
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/kipsigis_rs.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"RSma"]) | is.na(data[,"RSmidpointf"]) |
  is.na(data[,"age"]) | is.na(data[,"agef"]) | is.na(data[,"male"]))
data <- data[-drop,]
expect_true(nrow(data) == 270)
# 2009 paper used 270 individuals


## Create Variables and Recreate 2009 Results

rs <- data[,"RSma"]
rsfm <- data[,"RSmidpointf"]
age <- data[,"age"]
agef <- data[,"agef"]
male <- data[,"male"]
hhid <- data[,"fid"]

m0 <- lm(rs ~ rsfm + age + agef + male)

display.er(m0, cluster = hhid)
# Published Beta given here: 0.213 (0.106)
