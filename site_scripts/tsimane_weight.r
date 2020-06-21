
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_weight.csv", stringsAsFactors = FALSE)
drop <- which(duplicated(data[,"id"]))
drop <- c(drop, which(is.na(data[,"female"]) | is.na(data[,"weight"]) | is.na(data[,"age"])))
drop <- c(drop, intersect(which(rowSums(is.na(cbind(data[,"weightf"], data[,"agef"]))) > 0), which(rowSums(is.na(cbind(data[,"weightm"], data[,"agem"]))) > 0)))
drop <- c(drop, which(data[,"agem"] < 13 | data[,"agef"] < 13 | data[,"age"] < 18))
drop <- c(drop, which(data[,"agem"] - data[,"age"] < 13 | data[,"agef"] - data[,"age"] < 13))
drop <- c(drop, which(data[,"weight"] < 16))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 383)

## Create Variables and Recreate 2009 Results

weight <- data[,"weight"]
weightfm <-  rowMeans(data.frame(data[,"weightm"], data[,"weightf"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
female <- data[,"female"]
male <- 1- female
mom.only <- ifelse( is.na(data[,"weightf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"weightm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(weight ~ weightfm + age + age2 + female + dad.only + mom.only)

display.er(m0, cluster=hhid)
# * Published Beta here: 0.253 (0.069)

