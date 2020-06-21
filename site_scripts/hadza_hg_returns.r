
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/hadza_hg_returns.csv", stringsAsFactors = FALSE)
drop <- which(is.na(data[,"kcal"]*data[,"age"]))
drop <- c(drop, which(data[,"age"] < 15 | data[,"agem"] < 15 | data[,"agef"] < 15))
drop <- c(drop, which(is.na(data[,"kcalf"]*data[,"agef"]) & is.na(data[,"kcalm"]*data[,"agem"])))
data <- data[-drop,]
# expect_true(nrow(data) == 33)
# 2009 paper used 33 individuals, but there's big problems with that number (see errata).  

## Create Variables and Recreate 2009 Results

kcal <- data[,"kcal"]
kcalfm <- rowMeans(cbind(data[,"kcalf"], data[,"kcalm"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(cbind(data[,"agef"], data[,"agem"]), na.rm=T)
age2 <- age^2
age3 <- age^3
age4 <- age^4
parage2 <- parage^2
parage3 <- parage^3
parage4 <- parage^4
male <- ifelse(data[,"sex"]==1, 1, 0)
female <- 1-male
mom.only <- as.numeric(is.na(data[,"agef"]*data[,"kcalf"]))
dad.only <- as.numeric(is.na(data[,"agem"]*data[,"kcalm"]))
hhid <- paste("F", data[,"fid"], "M", data[,"mid"], sep="")

m0 <- lm(kcal ~ kcalfm + age + age2 + age3 + age4 + parage + parage2 + parage3 + parage4 + female + mom.only + dad.only)

display.er(m0, cluster=hhid)
# ** Published Beta: 0.047 (0.193)

