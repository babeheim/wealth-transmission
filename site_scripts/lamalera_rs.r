
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/lamalera_rs.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"rs"]) | is.na(data[,"age"]))
drop <- c(drop, intersect(which(rowSums(is.na(cbind(data[,"rsf"], data[,"agef"]))) > 0), which(rowSums(is.na(cbind(data[,"rsm"], data[,"agem"]))) > 0)))
data <- data[-drop,]
expect_true(nrow(data) == 121)

## Create Variables and Recreate 2009 Results

rs <- data[,"rs"]
rsfm <-  rowMeans(data.frame(data[,"rsm"], data[,"rsf"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
parage2 <- parage^2
male <- data[,"male"]
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(rs ~ rsfm + parage + parage2 + age + age2 + male)

display.er(m0, cluster=hhid)
# ** Published Beta here: 0.161 (0.174)
