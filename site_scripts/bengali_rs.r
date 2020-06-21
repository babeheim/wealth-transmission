
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/bengali_rs.csv", stringsAsFactors = FALSE)

drop <- which(data[,"schedcst"]==0)
drop <- c(drop, which(data[,"agem"] - data[,"age"] < 13))  
drop <- c(drop, which(is.na(data[,"agem"]) | is.na(data[,"age"]) | is.na(data[,"rsm"]) | is.na(data[,"rs"])))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 382)
# 382 values left, same as 2009 analysis


## Create Variables and Recreate 2009 Results

rs <- data[,"rs"]
rsm <- data[,"rsm"]
age <- data[,"age"]
agem <- data[,"agem"]
age2 <- age^2
agem2 <- agem^2
hhid <- data[,"mid"]

m0 <- lm(rs ~ rsm + age + age2 + agem + agem2)
display.er(m0, cluster=hhid)

# ** Published Beta: -0.074 (0.057)

display(m0, digits=3)
robust.se(m0, hhid)
display.cr(m0, cluster=hhid)
sdd(rsm)/sdd(rs)
mean(rsm)/mean(rs)

