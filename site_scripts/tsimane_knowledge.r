
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_knowledge.csv", stringsAsFactors = FALSE)
expect_true(nrow(data) == 181)
# 2009 paper uses 181 individuals

## Create Variables and Recreate 2009 Results

skill <- data[,"skillpcnt"]
skillfm <- rowMeans(data.frame(data[,"dadsskillpcnt"], data[,"momsskillpcnt"]), na.rm=T)
age <- data[,"age"]
age2 <- age^2
comunidad <- as.factor(data[,"comunidad"])
male <- data[,"male"]
mom.only <- ifelse(is.na(data[,"dadsskillpcnt"]*data[,"agef"]), 1, 0)
dad.only <- ifelse(is.na(data[,"momsskillpcnt"]*data[,"agem"]), 1, 0)
hhid <- paste("F",  data[,"dads_midpid"], "M",  data[,"moms_midpid"], sep="")

m0 <- lm(skill ~ skillfm + age + age2 + male + dad.only + mom.only + comunidad)

# display.er(m0, cluster=hhid)
# hmm display.er can't seem to handle categorical data...nevertheless this is right
#  * Published Beta here: -0.015 (0.097)
