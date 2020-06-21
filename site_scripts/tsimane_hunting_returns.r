
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_hunting_returns.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"agef"]) | is.na(data[,"agef"]) | is.na(data[,"rr"]) | is.na(data[,"rrf"]) | is.na(data[,"notrips"]) | is.na(data[,"notripsf"]))
drop <- c(drop, which(data[,"age"] < 15))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 26)
# 26 values left, same as 2009 analysis

## Create Variables and Recreate 2009 Results

rr <- data[,"rr"]
rrf <- data[,"rrf"]
age <- data[,"age"]
age2 <- age^2
agef <- data[,"agef"]
agef2 <- agef^2
village <- as.factor(data[,"village"])
num.trips <- data[,"notrips"]
num.tripsf <- data[,"notripsf"]

m00 <- lm(rr ~ rrf + age + age2 + agef + agef2 + village)
# I can't seem to recover the Stata command using frequency weights "[fw=notrips]" in R,
# so I'll have to replicate what it does manually.  
# It worries me how the standard error is different...

w.rr <- rep(rr, num.trips)
w.rrf <- rep(rrf, num.trips)
w.age <- rep(age, num.trips)
w.age2 <- rep(age2, num.trips)
w.agef <- rep(agef, num.trips)
w.agef2 <- rep(agef2, num.trips)
w.village <- rep(village, num.trips)

m0 <- lm(w.rr ~ w.rrf + w.age + w.age2 + w.agef + w.agef2 + w.village)
# *** Published Beta: 0.384 (0.130)
