
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/gambia_weight.csv", stringsAsFactors = FALSE)

## Clean Data Following 2009 Paper

drop <- which(is.na(data[,"sex"]) | is.na(data[,"weight"]) | is.na(data[,"age"]))
drop <- c(drop, intersect(which(rowSums(is.na(cbind(data[,"weightf"], data[,"agef"]))) > 0),
  which(rowSums(is.na(cbind(data[,"weightm"], data[,"agem"]))) > 0)))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 1274)
# 2009 paper used 1274 individuals


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
male <- ifelse(data[,"sex"]==2, 0, 1)
female <- 1-male
mom.only <- ifelse( is.na(data[,"weightf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"weightm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(weight ~ weightfm + age + age2 + age3 + age4 + parage + parage2 + parage3 + parage4 + female + dad.only + mom.only)
display.er(m0, cluster=hhid)
# Published Beta: 0.391 (0.041)
