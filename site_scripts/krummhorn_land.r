
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/krummhorn_land.csv", stringsAsFactors = FALSE)

expect_true(nrow(data) == 1602)
# 2009 paper used 1602 individuals

## Create Variables and Recreate 2009 Results

male <- ifelse(data[,"sex"]==1, 1, 0)
female <- 1-male
land <- data[,"land"]
landf <- data[,"landf"]
year.married <- data[,"uniony"]
year.married2 <- year.married^2
hhid <- data[,"idu"]

m0 <- lm(land ~ landf + female + year.married + year.married2)

display.er(m0, cluster=hhid)
#  ** Published Beta given here: 0.610 (0.043)

