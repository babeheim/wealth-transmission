
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/lamalera_food_sharing.csv", stringsAsFactors = FALSE)

drop <- which(is.na(data[,"outdegp"]))
data <- data[-drop,]
deg <- data[,"indeg"] + data[,"outdeg"]
degp <- data[,"indegp"] + data[,"outdegp"]
new.drop <- which(deg == 0 | degp == 0) # a concession to doing log-log transforms...
data <- data[-new.drop,]

expect_true(nrow(data) == 119)
# 2009 paper used 119 individuals, but two 0's were dropped to convert to log-log space...


## Create Variables and Recreate 2009 Results

deg <- data[,"indeg"] + data[,"outdeg"]
degp <- data[,"indegp"] + data[,"outdegp"]
male <- data[,"male"]
female <- 1-male
age <- data[,"age"]
age2 <- age^2
agem <- data[,"ageatdeathm"]
m.replace <- which(is.na(agem))
agem[m.replace] <- data[m.replace,"agetodaym"]
agef <- data[,"ageatdeathf"]
f.replace <- which(is.na(agef))
agef[f.replace] <- data[f.replace,"agetodayf"]
parage <- rowMeans(data.frame(agem, agef), na.rm=T)
parage2 <- parage^2
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")
ln.deg <- log(deg)
ln.degp <- log(degp)


m0 <- lm(deg ~ degp + age + age2 + parage + parage2 + female)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)


m00 <- lm(ln.deg ~ ln.degp + age + age2 + parage + parage2 + female)

bs.beta <- integer(0)

for(i in 1:100){
	boot.rows <- sample(1:length(ln.deg), replace=T)

	bs.ln.deg <- ln.deg[boot.rows]
	bs.ln.degp <- ln.degp[boot.rows]
	bs.female <- female[boot.rows]
	bs.age <- age[boot.rows]
	bs.age2 <- age2[boot.rows]
	bs.parage <- parage[boot.rows]
	bs.parage2 <- parage2[boot.rows]

	m000 <- lm(bs.ln.deg ~ bs.ln.degp + bs.age + bs.age2 + bs.parage + bs.parage2 + bs.female)
	bs.beta <- c(bs.beta, coef(m000)["bs.ln.degp"])
}

display(m00, digits=3)
robust.se(m00, hhid)
sd(bs.beta)
#  Published Beta: 0.251 (0.052)

