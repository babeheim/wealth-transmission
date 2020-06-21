
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/pimbwe_household_wealth.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"wealth"]))
drop <- c(drop, which(is.na(data[,"age"])))
drop <- c(drop, which(is.na(data[,"wealthm"]) & is.na(data[,"wealthf"])))
drop <- c(drop, which(is.na(data[,"agem"]) & is.na(data[,"agef"])))
data <- data[-drop,]
expect_true(nrow(data) == 283)

## Create Variables and Recreate 2009 Results
	
female <- ifelse( data[,"sex"]=="f", 1, 0 )
male <- 1-female
mom.only <- ifelse( is.na(data[,"wealthf"]), 1, 0)
dad.only <- ifelse( is.na(data[,"wealthm"]), 1, 0) 
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")
wealth <- data[,"wealth"]
ln.wealth <- log(wealth)
wealthfm <- rowMeans(data.frame(data[,"wealthf"], data[,"wealthm"]), na.rm=T)
ln.wealthfm <- log(wealthfm)
age <- data[,"age"]
age.two <- age^2
age.three <- age^3
age.four <- age^4
parage <- rowMeans(data.frame(data[,"agem"], data[,"agef"]), na.rm=T)
parage.two <- parage^2
parage.three<- parage^3
parage.four <- parage^4


m0 <- lm(wealth ~ wealthfm + age + age.two + age.three + age.four + parage +
  parage.two + parage.three + parage.four + female + mom.only + dad.only)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)


	
m00 <- lm(ln.wealth ~ ln.wealthfm + age + age.two + age.three + age.four +
  parage + parage.two + parage.three + parage.four + female + mom.only + dad.only)
	
display(m00, digits=3)
robust.se(m00, hhid)
# Published Beta: 0.107 (0.318)   
