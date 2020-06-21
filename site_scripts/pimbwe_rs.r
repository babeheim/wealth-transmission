
rm(list = ls())

source("./project_support.r")

###########

## Clean Data Following 2009 Paper

data <- read.csv("./data/pimbwe_rs.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"rs"]))
drop <- c(drop, which(is.na(data[,"age"])))
drop <- c(drop, which(is.na(data[,"rsm"]) & is.na(data[,"rsf"])))
drop <- c(drop, which(is.na(data[,"agem"]) & is.na(data[,"agef"])))
data <- data[-drop,]
expect_true(nrow(data) == 599)
# 2009 paper used 599 individuals


## Create Variables and Recreate 2009 Results
male <- ifelse( data[,"sex"]=="m", 1, 0 )
mom.only <- ifelse( is.na(data[,"rsf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"rsm"]), 1, 0) 
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")
rs <- data[,"rs"]
age <- data[,"age"]
rsfm <- rowMeans(data.frame(data[,"rsm"], data[,"rsf"]), na.rm=T)
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
age2 <- age^2
parage2 <- parage^2
	
m0 <- lm(rs ~ rsfm + age + age2 + parage + parage2 + male + mom.only + dad.only)

display.er(m0, cluster=hhid)
# Published Results: 
# Beta: -0.057 (0.107)
# ...note that this SE is actually the non-cluster-robust value; did they report it as cluster-robust?
