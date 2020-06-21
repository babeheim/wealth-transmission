
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/hadza_weight.csv", stringsAsFactors = FALSE)
drop <- which(duplicated(data[,"id"]))
drop <- c(drop, which(is.na(data[,"sex"]) | is.na(data[,"weight"]) | is.na(data[,"age"])))
drop <- c(drop, intersect(which(is.na(data[,"weightf"]*data[,"agef"])), which(is.na(data[,"weightm"]*data[,"agem"]))))
drop <- c(drop, which(data[,"agem"] < 13 | data[,"agef"] < 13 | data[,"age"] < 18))
drop <- c(drop, which(data[,"agem"] - data[,"age"] < 13 | data[,"agef"] - data[,"age"] < 13))
drop <- sort(unique(drop))
data <- data[-drop,]
# expect_true(nrow(data) == 227)
# 2009 paper has 227 individuals...I can't explain why I don't see 5 of them


## Create Variables and Recreate 2009 Results

weight <- data[,"weight"]
weightfm <-  rowMeans(data.frame(data[,"weightm"], data[,"weightf"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
age3 <- age^3
age4 <- age^4
parage2 <- parage^2
parage3 <- parage^3
parage4 <- parage^4
female <- ifelse(data[,"sex"]==2,1,0)
male <- 1- female
mom.only <- ifelse( is.na(data[,"weightf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"weightm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(weight ~ weightfm + age + age2 + parage + parage2 + parage3 + parage4 + female + dad.only + mom.only)
display.er(m0, cluster=hhid)
# Published Beta: 0.253 (0.069)
