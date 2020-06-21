
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/khasi_rs.csv", stringsAsFactors = FALSE)

drop <- which(data[,"age"] < 18 | data[,"agem"] - data[,"age"] < 13)
drop <- c(drop, which(is.na(data[,"rs"])))
drop <- sort(unique(drop))
data <- data[-drop,]
expect_true(nrow(data) == 650)
# 650 values left, same as 2009 analysis

## Create Variables and Recreate 2009 Results

rs <- data[,"rs"]
rsm <- data[,"rsm"]

age <- data[,"age"]
age2 <- age^2

agem <- data[,"agem"]
agem2 <- agem^2
mid <- data[,"mid"]

m0 <- lm(rs ~ rsm + age + age2 + agem + agem2)

display.er(m0, cluster=mid)

# Published Beta: 0.165 (0.045)


