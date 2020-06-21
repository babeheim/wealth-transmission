
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/datoga_rs.csv", stringsAsFactors = FALSE)

drop <- which(data[,"agef"] - data[,"age"] < 13)
drop <- c(drop, which(is.na(data[,"rs"])))
drop <- sort(unique(drop))
data <- data[-drop,]
# expect_true(nrow(data) == 133))
# 133 used in 2009 analysis


## Create Variables and Recreate 2009 Results

male <- ifelse(data[,"sex"]=="s", 1, 0)
rs <- data[,"rs"]
rsf <- data[,"rsf"]
age <- data[,"age"]
agef <- data[,"agef"]
age2 <- age^2
agef2 <- agef^2
hhid <- as.character(data[,"fid"])

m0 <- lm(rs ~ rsf + age + age2 + agef + agef2 + male)
display.er(m0, cluster=hhid)
# Published Beta: 0.066 (0.060)
