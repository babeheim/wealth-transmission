
rm(list = ls())

source("./project_support.r")

data <- read.dta("./input/Meriam_Data.dta")
data.o <- data

## Clean Data Following 2009 Paper

data <- data.o
drop <- which(data[,"id"]=="")
agef.cleaning <- data[,"age_fa"]
agef.cleaning[agef.cleaning=="dec."] <- 75
agef.cleaning <- as.numeric(agef.cleaning)
drop <- c(drop, which(!is.na(data[,"rsf"]) & is.na(agef.cleaning)))
drop <- c(drop, which(!is.na(data[,"rsm"]) & is.na(data[,"agem"])))
data <- data[-drop, ]
agef.cleaning <- agef.cleaning[-drop]
drop <- which( agef.cleaning - data[,"age"] < 13 | data[,"agem"] - data[,"age"] < 13)
data <- data[-drop,]
expect_true(nrow(data) == 91)
# 2009 paper used 91 individuals


## Create Variables and Recreate 2009 Results

rs <- data[,"rs"]
rsfm <- rowMeans(data.frame(data[,"rsm"], data[,"rsf"]), na.rm=T)
male <- ifelse(data[,"sex"]=="M", 1, 0)
agef <- data[,"age_fa"]
agef[agef=="dec."] <- 75
agef <- as.numeric(agef)
age <- data[,"age"]
age2 <- age^2
mom.only <- ifelse( is.na(data[,"rsf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"rsm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(rs ~ rsfm + age + age2 + male + dad.only + mom.only)

display.er(m0, cluster=hhid)
# ** Published Beta: 0.088 (0.247)
