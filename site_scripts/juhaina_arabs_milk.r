
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/juhaina_arabs_milk.csv", stringsAsFactors = FALSE)

expect_true(nrow(data) == 21)
# 2009 results have 21 individuals

## Create Variables and Recreate 2009 Results

female <- data[,"sex"]
milk <- data[,"milkcollected"]
age <- data[,"age"]
age2 <- age^2
milkf <- data[,"milkcollectedf"]
agef <- data[,"agef"]
fid <- data[,"fid"]
ln.milk <- log(milk)
ln.milkf <- log(milkf)

m0 <- lm(milk ~ milkf + age + age2 +  female)
precis(m0)
robust.se(m0, cluster=fid)
display.er(m0, cluster=fid)
display.cr(m0, cluster=fid)

m00 <- lm(ln.milk ~ ln.milkf + age + age2 + female)

bs.beta <- integer(0)
for(i in 1:100){
	boot.rows <- sample(1:length(ln.milk), replace=T)
	bs.ln.milk <- ln.milk[boot.rows]
	bs.ln.milkf <- ln.milkf[boot.rows]
	bs.female <- female[boot.rows]
	bs.age <- age[boot.rows]
	bs.age2 <- age2[boot.rows]
	
	m000 <- lm(bs.ln.milk ~ bs.ln.milkf + bs.age + bs.age2 + bs.female)
	
	bs.beta <- c(bs.beta, coef(m000)["bs.ln.milkf"])
}

display(m00, digits=3)
robust.se(m00, fid)
# Published Beta here: 0.535 (0.226)
# well, the standard error isn't the same but it is in the same ballpark...


