
rm(list = ls())

source("./project_support.r")

data <- read.csv("./data/tsimane_rs.csv", stringsAsFactors = FALSE)

drop <- which(data[,"age"] < 18 | is.na(data[,"age"]))
data <- data[-drop,]
drop <- which(data[,"agef"] - data[,"age"] < 13 | data[,"agem"] - data[,"age"] < 13)
data <- data[-drop,]
drop <- which(is.na(data[,"agef"]*data[,"rsf"]) & is.na(data[,"agem"]*data[,"rsm"]))
data <- data[-drop,]
drop <- which(data[,"rs"]==0)
data <- data[-drop,]
expect_true(nrow(data) == 682)
# 2009 paper reports it uses 849 individuals,
# but because it uses log-log transformations
# all the RS=0 ppl were dropped and the analysis really only used 682


## Create Variables and Recreate 2009 Results
rs <- data[,"rs"]
age <- data[,"age"]
rsfm <- rowMeans(cbind(data[,"rsf"], data[,"rsm"]), na.rm=T)
parage <- rowMeans(cbind(data[,"agef"], data[,"agem"]), na.rm=T)
hhid <- paste("F", data[,"fid"], "M", data[,"mid"])
male <- ifelse(data[,"male"]==1, 1, 0)
female <- 1-male
mom.only <- as.numeric(is.na(data[,"rsf"]*data[,"agef"]))
dad.only <- as.numeric(is.na(data[,"rsm"]*data[,"agem"]))
age2 <- age^2
parage2 <- parage^2
ln.rs <- log(rs) # but theer's a bunch of 0s...
ln.rsfm <- log(rsfm)

m0 <- lm(rs ~ rsfm + age + age2 + parage + parage2 + male + mom.only + dad.only)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)


m00 <- lm(ln.rs ~ ln.rsfm + age + age2 + parage + parage2 + male + mom.only + dad.only)
display(m00)
robust.se(m00, hhid)
# Published Beta: 0.128 (0.073)
# I'm not sure exactly why the numbers are what they are here.
# In any case, if I run the m00 specification in Stata I get pretty much the same numbers.  
