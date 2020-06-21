
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/kipsigis_cattle_partners.csv", stringsAsFactors = FALSE)

## Clean Data Following 2009 Paper

drop <- which(is.na(data[,"partners"]*data[,"partnersf"]*data[,"agef"]))
data <- data[-drop,]
expect_true(nrow(data) == 102)
# 2009 paper has 102 entries


## Create Variables and Recreate 2009 Results

partners <- data[,"partners"]
partnersf <- data[,"partnersf"]
age <- data[,"age"]
age2 <- age^2
age3 <- age^3
age4 <- age^4
agef <- data[,"agef"]
agef2 <- agef^2
agef3 <- agef^3
agef4 <- agef^4
male <- data[,"male"]
female <- 1-male

hhid <- data[,"fid"]

m0 <- lm(partners ~ partnersf + age + age2 + age3 + age4 + agef + agef2 + agef3 + agef4 + female)
display.er(m0, cluster=hhid)
# ** Published Beta here: 0.041 (0.139)
# hmm..the SE's on the other variables are very different from the STATA code
