
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/east_anglia_estate_value.csv", stringsAsFactors = FALSE)

drop <- which(data[,"assets"] < 1 | data[,"estatevaluef"] < 1)
# drop <- c(drop, which(data[,"estatevaluef"] < exp(4)))   # I can't see what justifies this
drop <- c(drop, which(is.na(data[,"age"]) | is.na(data[,"agef"])))
drop <- sort(unique(drop))
data <- data[-drop,]
# expect_true(nrow(data) == 210)
# 210 values in the 2009 analysis, using the weird exp(4) cutoff for F1 wealth.  

## Create Variables and Recreate 2009 Results

wealth <- data[,"assets"]
wealthf <- data[,"estatevaluef"]
ln.wealth <- log(wealth)
ln.wealthf <- log(wealthf)

age <- data[,"age"]
age2 <- age^2
age3 <- age^3
age4 <- age^4

agef <- data[,"agef"]
agef2 <- agef^2
agef3 <- agef^3
agef4 <- agef^4

m0 <- lm(ln.wealth ~ ln.wealthf + age + age2 + age3 + age4 + agef + agef2 + agef3 + agef4)
# *** Published Beta: 0.642 (0.073)
summary(m0)
