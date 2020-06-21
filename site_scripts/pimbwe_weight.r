
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/pimbwe_weight.csv", stringsAsFactors = FALSE)
drop <- which(is.na(data[,"age"]))
drop <- c(drop, which(is.na(data[,"weightm"]) & is.na(data[,"weightf"])))
drop <- c(drop, which(is.na(data[,"agem"]) & is.na(data[,"agef"])))
drop <- c(drop, which(data[,"age"] < 18))
drop <- c(drop, which(data[,"weight"] < 20 | data[,"weight"] > 150))
data <- data[-drop,]
expect_true(nrow(data) == 148)


## Create Variables and Recreate 2009 Results

weight <- data[,"weight"]
weightfm <- rowMeans(data.frame(data[,"weightm"], data[,"weightf"]), na.rm=T)
age <- data[,"age"]
age2 <- age^2
age3 <- age^3
age4 <- age^4
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
parage2 <- parage^2
parage3 <- parage^3
parage4 <- parage^4
female <- ifelse(data[,"sex"]=="F", 1, 0)
male <- 1-female
mom.only <- ifelse( is.na(data[,"weightf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"weightm"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(weight ~ weightfm + age + age2 + age3 + age4 + parage + parage2 +
  parage3 + parage4 + female + dad.only + mom.only)

display.er(m0, cluster=hhid)
# Published Beta: 0.377 (0.096)
