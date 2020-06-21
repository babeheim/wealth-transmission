
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_household_wealth.csv", stringsAsFactors = FALSE)
drop <- which(data[, "age"]<18 | is.na(data[,"age"]*data[,"wealth"]))
drop <- c(drop, which(is.na(data[,"wealthm"]) & is.na(data[,"wealthf"])))
data <- data[-drop,]
drop <- which(duplicated(data[,"id"]))
data <- data[-drop,]
wealthfm <- rowMeans(cbind(data[,"wealthf"], data[,"wealthm"]), na.rm=T)
drop <- which(wealthfm==0)
data <- data[-drop,]
expect_true(nrow(data) == 110)
# 2009 paper uses 110 individuals

## Create Variables and Recreate 2009 Results

wealth <- data[,"wealth"]
wealthfm <- rowMeans(cbind(data[,"wealthf"], data[,"wealthm"]), na.rm=T)
age <- data[,"age"]
age2 <- age^2
parage <- rowMeans(cbind(data[,"agef"], data[,"agem"]), na.rm=T)
parage2 <- parage^2
female <- data[,"female"]
male <- 1-female
ln.wealth <- log(wealth)
ln.wealthfm <- log(wealthfm)

hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")

m0 <- lm(wealth ~ wealthfm + age + age2 + parage + parage2 + female)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)


m00 <- lm(ln.wealth ~ ln.wealthfm + age + age2 + parage + parage2 + female)
display(m00, digits=5)
robust.se(m00, hhid)
# * Published Beta is 0.024 (0.071)
# if you just do a simple log-log regression you get: 0.097 (0.109) in STATA 
# in R, I get something like: 0.085 (0.108). This coefficient difference seems mostly
# due to the weird rounding error when I load the data in with read.dta. 
# Nevertheless, its very similar. I just have no idea why the bootstrap beta is so different!  
