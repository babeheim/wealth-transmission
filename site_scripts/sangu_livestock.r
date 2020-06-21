
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/sangu_livestock.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"livestock"]*data[,"livestockf"]))
data <- data[-drop,]
expect_true(nrow(data) == 108)
# 2009 paper used 108 individuals

## Create Variables and Recreate 2009 Results
livestock <- data[,"livestock"]
male <- ifelse(data[,"sex"]=="M", 1, 0)
female <- 1-male
livestockf <- data[,"livestockf"]
age <- data[,"age"]
age2 <- age^2

m0 <- lm(livestock ~ livestockf + female + age + age2)

#  ** Published Beta given here: 0.957 (0.424)
# I'm way off on the SE because I'm not bootstrapping, but otherwise its the same
