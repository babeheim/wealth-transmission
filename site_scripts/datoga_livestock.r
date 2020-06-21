
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/datoga_livestock.csv", stringsAsFactors = FALSE)

## Clean Data Following 2009 Paper

drop <- which(data[,"agef"] - data[,"age"] < 13)
drop <- c(drop, which(is.na(data[,"livestock"])))
drop <- sort(unique(drop))
data <- data[-drop,]
# expect_true(nrow(data) == 135)
# 2009 paper had 135 individuals

## Create Variables and Recreate 2009 Results

male <- ifelse(data[,"sex"]=="s", 1, 0)
livestock <- data[,"livestock"]
livestockf <- data[,"livestockf"]
age <- data[,"age"]
agef <- data[,"agef"]
age2 <- age^2
agef2 <- agef^2
hhid <- as.character(data[,"fid"])

m0 <- lm(livestock ~ livestockf + age + age2 + agef + agef2 + male)
display(m0, digits=3)
robust.se(m0, hhid)
# * Published Beta: 0.622 (0.127) 
# 2009 paper used "errors in variables regression", which I can't replicate in R
# notably they didn't cluster on hhid...
