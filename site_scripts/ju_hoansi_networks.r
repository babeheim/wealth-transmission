
rm(list = ls())

source("./project_support.r")

data <- read.dta("./input/Weissner_Data.dta")


## Clean Data Following 2009 Paper

data <- read_csv("ju/'hoa

drop <- which(data[,"age"]<14)
data <- data[-drop,]
expect_true(nrow(data) == 26)
# 2009 paper used 26 observations


## Create Variables and Recreate 2009 Results

male <- data[,"male"]
female <- 1-male
network <- data[,"hp"]
age <- data[,"age"]
age2 <- age^2

networkfm <- rowMeans(data.frame(data[,"hpf"], data[,"hpm"]), na.rm=T)
networkfm.sum <- rowSums(data.frame(data[,"hpf"], data[,"hpm"]), na.rm=T)
parage <- rowMeans(data.frame(data[,"agef"], data[,"agem"]), na.rm=T)
parage2 <- parage^2
mom.only <- ifelse(is.na(data[,"hpf"]*data[,"agef"]), 1, 0)
dad.only <- ifelse(is.na(data[,"hpm"]*data[,"agem"]), 1, 0)
hhid <- paste("F",  data[,"fid"], "M",  data[,"mid"], sep="")



m0 <- lm(network ~ networkfm.sum + age + age2 + parage + parage2 + male)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)




ln.network <- log(network)
ln.networkfm <- log(networkfm)
ln.networkfm.sum <- log(networkfm.sum)

# why use the SUM of the parents' values as the wealth measure?

m00 <- lm(ln.network ~ ln.networkfm.sum + age + age2 + parage + parage2 + male)

bs.beta <- integer(0)

for(i in 1:100){
	boot.rows <- sample(1:length(ln.network), replace=T)

	bs.ln.network <- ln.network[boot.rows]
	bs.ln.networkfm.sum <- ln.networkfm.sum[boot.rows]
	bs.male <- male[boot.rows]
	bs.age <- age[boot.rows]
	bs.age2 <- age2[boot.rows]
	bs.parage <- parage[boot.rows]
	bs.parage2 <- parage2[boot.rows]

	m000 <- lm(bs.ln.network ~ bs.ln.networkfm.sum + bs.age + bs.age2 + bs.parage + bs.parage2 + bs.male)
	
	bs.beta <- c(bs.beta, coef(m000)["bs.ln.networkfm.sum"])
}

display(m00, digits=3)
robust.se(m00, hhid)
sd(bs.beta)
# Published Beta here: 0.208 (0.114)
