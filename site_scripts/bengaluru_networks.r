
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

d <- read.csv("./data/bengaluru_networks.csv", stringsAsFactors = FALSE)

too_young <- which(d$age < 30)
d <- d[-too_young, ]
age_mismatch <- which(d$agep - d$age < 13)
d <- d[-age_mismatch, ]
drop_second_parent <- which(duplicated(d$id))
d <- d[-drop_second_parent, ]
wealth_missing <- which(is.na(d$networkwealth))
d <- d[-wealth_missing, ]

expect_true(nrow(d) == 249)
# 2009 paper had 249 cases

## Create Variables and Recreate 2009 Results

wealth <- d[,"networkwealth"]
wealthp <- d[,"networkwealthp"]

hhid <- d[,"hhid"]
male <- ifelse(d[,"sex"]==1, 1, 0)
female <- 1-male

# OLS and elasticity at means:

m0 <- lm(wealth ~ wealthp + female)
expect_true(abs(logLik(m0) - (-720)) < 1)
precis(m0)
robust.se(m0, cluster=hhid)
display.er(m0, cluster=hhid)
display.cr(m0, cluster=hhid)
sdd(wealthp)/sdd(wealth)

ln.wealth <- log(wealth)
ln.wealthp <- log(wealthp)

m00 <- lm(ln.wealth ~ ln.wealthp + female)

bs.beta <- integer(0)

for(i in 1:1000){
	boot.rows <- sample(1:length(ln.wealth), replace=T)

	bs.ln.wealth <- ln.wealth[boot.rows]
	bs.ln.wealthp <- ln.wealthp[boot.rows]
	bs.female <- female[boot.rows]

	m000 <- lm(bs.ln.wealth ~ bs.ln.wealthp + bs.female)
	bs.beta <- c(bs.beta, coef(m000)["bs.ln.wealthp"])
}
display(m00, digits=3)
sd(bs.beta)

# Published Beta:  0.114 (0.073)
