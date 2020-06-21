
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/lamalera_house_quality.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"wealth"]*data[,"wealthp"]*data[,"age"]))
data <- data[-drop,]
expect_true(nrow(data) == 121)
# 2009 paper used 121 individuals


## Create Variables and Recreate 2009 Results

wealth <- data[,"wealth"]
wealthp <- data[,"wealthp"]
age <- data[,"age"]
age2 <- age^2
age3 <- age^3
age4 <- age^4
agem <- data[,"ageatdeathm"]
m.replace <- which(is.na(agem))
agem[m.replace] <- data[m.replace,"agetodaym"]
agef <- data[,"ageatdeathf"]
f.replace <- which(is.na(agef))
agef[f.replace] <- data[f.replace,"agetodayf"]
parage <- rowMeans(data.frame(agem, agef), na.rm=T)
parage2 <- parage^2
parage3 <- parage^3
parage4 <- parage^4
male <- data[,"male"]
female <- 1-male
hhid <- data[,"hh"]

m0 <- lm(wealth ~ wealthp + age + age2 + age3 + age4 + parage + parage2 + parage3 + parage4 + female)
display.er(m0, cluster=hhid)
# ** Published Beta here: 0.218 (0.099)

