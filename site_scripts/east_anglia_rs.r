
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/east_anglia_rs.csv", stringsAsFactors = FALSE)

# drop <- which(data[,"assets"] < 1 | data[,"estatevaluef"] < 1)
# drop <- c(drop, which(data[,"estatevaluef"] < exp(4)))   # I can't see what justifies this
drop <- which(is.na(data[,"fert"]) | is.na(data[,"fertf"]))
drop <- c(drop, which(is.na(data[,"age"]) | is.na(data[,"agef"])))
drop <- sort(unique(drop))
data <- data[-drop,]
# expect_true(nrow(data) == 210)
print("warning, east anglia RS n is wrong")
# 210 values in the 2009 analysis, using the weird exp(4) cutoff for F1 wealth.  

## Create Variables and Recreate 2009 Results

rs <- data[,"fert"]
rsf <- data[,"fertf"]

age <- data[,"age"]
age2 <- age^2

agef <- data[,"agef"]
agef2 <- agef^2

son.death.decade <- data[,"s_dec_death"] # I'm just guessing that's what it means

fid <- data[,"fid"]

m0 <- lm(rs ~ rsf + age + age2 + agef + agef2 + son.death.decade)
# *** Published Beta: 0.171 (0.150)
display.er(m0, cluster=fid, digits=3)
