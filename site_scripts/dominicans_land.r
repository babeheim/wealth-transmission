
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/dominicans_land.csv", stringsAsFactors = FALSE)
drop <- which(is.na(data[,"wealth"]))
drop <- c(drop, which(is.na(data[,"wealthf"])))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 62)
# 2009 paper had 62 individuals

## Create Variables and Recreate 2009 Results

wealth <- data[,"wealth"]
male <- data[,"male"]
wealthf <- data[,"wealthf"]
age <- as.numeric(data[,"age"])
age[age>90] <- 90
hhid <- data[,"fid"]
m0 <- lm(wealth ~ wealthf + male + age)
display.er(m0, cluster=hhid)
# They used the "errors in values" regression in Stata, which I can't replicate in R.
# However, these are the same variables, for what it is worth.
# ** Published Beta: 0.137 (0.140)
