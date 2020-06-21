
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_allies.csv", stringsAsFactors = FALSE)
drop <- which(is.na(data[,"alliesf"]))
data <- data[-drop,]
expect_true(nrow(data) == 45)
# 45 values left, same as 2009 analysis

## Create Variables and Recreate 2009 Results

allies <- data[,"allies"]
alliesf <- data[,"alliesf"]
age <- data[,"age"]
age2 <- age^2
agef <- data[,"agef"]

m0 <- lm(allies ~ alliesf + age + age2)
# *** Published Beta: 0.338 (0.103)

display.er(m0)
display(m0, digits=3)
display.cr(m0)
sdd(alliesf)/sdd(allies)
mean(alliesf)/mean(allies)
