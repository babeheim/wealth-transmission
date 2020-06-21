
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/hadza_grip_strength.csv", stringsAsFactors = FALSE)
drop <- which(data[,"age"] < 18 | data[,"agem"] < 13 | data[,"agef"] < 13)
data <- data[-drop,]
drop <- which(data[,"agem"] - data[,"age"] < 13 | data[,"agef"] - data[,"age"] < 13)
data <- data[-drop,]
drop <- which(is.na(data[,"gripr"]) | is.na(data[,"age"]) | is.na(data[,"sex"]))
drop <- c(drop, intersect(which(rowSums(is.na(cbind(data[,"griprf"], data[,"agef"]))) > 0),
  which(rowSums(is.na(cbind(data[,"griprm"], data[,"agem"]))) > 0)))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 196)
# 2009 Paper used 196 individuals

## Create Variables and Recreate 2009 Results

gripr <- data[,"gripr"]
griprfm <-  rowMeans(data.frame(data[,"griprf"], data[,"griprm"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
parage2 <- parage^2
female <- ifelse(data[,"sex"]==2, 1, 0)
male <- 1-female
mom.only <- ifelse( is.na(data[,"griprf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"griprm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(gripr ~ griprfm + age + age2 + parage + parage2 + female + dad.only + mom.only)
display.er(m0, cluster=hhid)
# ** Published Beta:  -0.044 (0.050)
