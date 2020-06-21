
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/pimbwe_farming_skill.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"rawskill"]*data[,"age"]*data[,"hectares"]))
drop <- c(drop, which(is.na(data[,"rawskillf"]*data[,"agef"]*data[,"hectaresf"]) &
  is.na(data[,"rawskillm"]*data[,"agem"]*data[,"hectaresm"])))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 217)
# 2009 paper used 217 individuals


## Create Variables and Recreate 2009 Results

rawskill <- data[,"rawskill"]
rawskillfm <- rowMeans(data.frame(data[,"rawskillf"], data[,"rawskillm"]), na.rm=T)
hectares <- data[,"hectares"]
hectaresfm <- rowMeans(data.frame(data[,"hectaresf"], data[,"hectaresm"]), na.rm=T)
age <- data[,"age"]
age2 <- age^2
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
parage2 <- parage^2
male <- ifelse(data[,"sex"]=="m",1, 0)
female <- 1-male
mom.only <- ifelse(is.na(data[,"rawskillf"]*data[,"agef"]*data[,"hectaresf"]), 1, 0)
dad.only <- ifelse(is.na(data[,"rawskillm"]*data[,"agem"]*data[,"hectaresm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(rawskill ~ rawskillfm + age + age2 + parage + parage2 + female + dad.only +
  mom.only + hectares + hectaresfm)

display.er(m0, cluster=hhid)
#  * Published Beta here: -0.015 (0.097)

