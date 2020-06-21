
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/gambia_rs.csv", stringsAsFactors = FALSE)

drop <- which(data[,"yrbirth"] > 1960 | is.na(data[,"yrbirth"]))  
drop <- c(drop, which(data[,"age"] < 15))
drop <- c(drop, which(is.na(data[,"age"]) | is.na(data[,"rs"]) | is.na(data[,"sex"])))
drop <- c(drop, which(is.na(data[,"rsm"]) & is.na(data[,"rsf"])))
drop <- c(drop, which(is.na(data[,"agef"]) & !is.na(data[,"rsf"]))) # potential mistake here?
drop <- c(drop, which(is.na(data[,"agem"]) & !is.na(data[,"rsm"]))) # potential mistake here?
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 967)
# 2009 paper used 967 people


## Create Variables and Recreate 2009 Results

sex <- data[,"sex"]
female <- ifelse(data[,"sex"]=="female", 1, 0)
male <- 1- female
mom.only <- ifelse( is.na(data[,"rsf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"rsm"]), 1, 0)
rs <- data[,"rs"]
rsfm <- rowMeans(data.frame(data[,"rsm"], data[,"rsf"]), na.rm=T)
age <- data[,"age"]
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
parage2 <- parage^2
yrbirth <- data[,"yrbirth"]
paryear <- rowMeans(data.frame(data[,"mtyrbir"], data[,"ftyrbir"]), na.rm=T)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")
fcen1 <- ifelse(data[,"ftcensor"]=="died before 1975", 1, 0)
fcen1[is.na(fcen1)] <- 0
fcen2 <- ifelse(data[,"ftcensor"]=="censored before 1975", 1, 0)
fcen2[is.na(fcen2)] <- 0
fcen3 <- ifelse(data[,"ftcensor"]=="censored 1.1.75", 1, 0)
fcen3[is.na(fcen3)] <- 0
fcen4 <- ifelse(is.na(data[,"ftcensor"]), 1, 0)
mcen1 <- ifelse(data[,"mtcensor"]=="died before 1975", 1, 0)
mcen1[is.na(mcen1)] <- 0
mcen2 <- ifelse(data[,"mtcensor"]=="censored before 1975", 1, 0)
mcen2[is.na(mcen2)] <- 0
mcen3 <- ifelse(data[,"mtcensor"]=="censored 1.1.75", 1, 0)
mcen3[is.na(mcen3)] <- 0
mcen4 <- ifelse(is.na(data[,"mtcensor"]), 1, 0)
kcen1 <- ifelse(data[,"censor75"]=="died before 1975", 1, 0)
kcen1[is.na(kcen1)] <- 0
kcen2 <- ifelse(data[,"censor75"]=="censored before 1975", 1, 0)
kcen2[is.na(kcen2)] <- 0
kcen3 <- ifelse(data[,"censor75"]=="censored 1.1.75", 1, 0)
kcen3[is.na(kcen3)] <- 0
kcen4 <- ifelse(is.na(data[,"censor75"]), 1, 0)
i.village <- ifelse(data[,"village"]=="keneba", 1, 0)
 
m0 <- lm(rs ~ rsfm + age + age2 + parage + parage2 + yrbirth + paryear + female + dad.only + mom.only + i.village + fcen2 + fcen3 + fcen4 + mcen2 + mcen3 + mcen4 + kcen2 + kcen3 + kcen4)
 
display.er(m0, cluster=hhid, digits=6)
# Published Beta: 0.088 (0.086)
