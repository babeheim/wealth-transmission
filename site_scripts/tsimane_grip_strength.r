
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_grip_strength.csv", stringsAsFactors = FALSE)

drop <- which(data[,"mage"] < 18 | data[,"magem"] < 13 | data[,"magef"] < 13)
data <- data[-drop,]
drop <- which(data[,"magem"] - data[,"mage"] < 13 | data[,"magef"] - data[,"mage"] < 13)
data <- data[-drop,]
drop <- which(is.na(data[,"mano"]) | is.na(data[,"mage"]) | is.na(data[,"male"]))
drop <- c(drop, intersect(which(rowSums(is.na(cbind(data[,"manof"], data[,"magef"]))) > 0), which(rowSums(is.na(cbind(data[,"manom"], data[,"magem"]))) > 0)))
drop <- sort(unique(drop)) 
data <- data[-drop,]
expect_true(nrow(data) == 490)
# 2009 paper reported 490 individuals, but 113 of the rows are duplications...

## Create Variables and Recreate 2009 Results

mano <- data[,"mano"]
manofm <-  rowMeans(data.frame(data[,"manof"], data[,"manom"]), na.rm=T)
age <- data[,"mage"]
parage <- rowMeans(data.frame(data[,"magem"], data[,"magef"]), na.rm=T)

age2 <- age^2
age3 <- age^3
age4 <- age^4

parage2 <- parage^2
parage3 <- parage^3
parage4 <- parage^4

male <- data[,"male"]
female <- 1-male
mom.only <- ifelse( is.na(data[,"manof"]), 1, 0)
dad.only <- ifelse( is.na(data[,"manom"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(mano ~ manofm + age + age2 + age3 + age4 + parage + parage2 + parage3 + parage4 + female + dad.only + mom.only)
display.er(m0, cluster=hhid)
# ** Published Beta given here: 0.070 (0.042) 
# ** Beta without dups: 0.055 (0.048) 


