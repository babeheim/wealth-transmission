
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/skelleftea_rs.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"rs"]) | (is.na(data[,"rsm"]) & is.na(data[,"rsf"])))
data <- data[-drop,]
expect_true(nrow(data) == 2515)
# 2515 values left, same as 2009 analysis

## Create Variables and Recreate 2009 Results

rs <- data[,"rs"]
rsm <- data[,"rsm"]
rsf <- data[,"rsf"]

mom.only <- is.na(data[,"rsf"])
dad.only <- is.na(data[,"rsm"])

rsfm <- rep(NA, length(rs))
rsfm[is.na(rsm)] <- rsf[is.na(rsm)]
rsfm[is.na(rsf)] <- rsm[is.na(rsf)]
both <- which(!is.na(rsf) & !is.na(rsm))
rsfm[both] <- apply(cbind(rsm[both], rsf[both]), 1, mean)

age <- data[,"age"]
age2 <- age^2

m0 <- lm(rs ~ rsfm + age + age2 + mom.only + dad.only)
# *** Published Beta: 0.010 (0.028)

display.er(m0)
display(m0, digits=3)
display.cr(m0)
sdd(rsfm)/sdd(rs)
mean(rsfm)/mean(rs)
