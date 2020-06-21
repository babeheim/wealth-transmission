
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/lamalera_boat_shares.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"shares06"]*data[,"shares06p"]*data[,"age"]))
data <- data[-drop,]
expect_true(nrow(data) == 121)
# 2009 paper used 121 individuals


## Create Variables and Recreate 2009 Results

shares <- data[,"shares06"]
sharesp <- data[,"shares06p"]
age <- data[,"age"]
age2 <- age^2
male <- data[,"male"]
female <- 1-male
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(shares ~ sharesp + age + age2 + male)

display.er(m0, cluster=hhid)
# * Published Beta here: 0.122 (0.093)
