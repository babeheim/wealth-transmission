
rm(list = ls())

source("./project_support.r")

# load data

d <- read.csv("./data/ache_weight.csv", stringsAsFactors = FALSE)

# drop cases

age_invalid <- which(is.na(d$age))
d <- d[-age_invalid, ]

no_parent_info <- which(is.na(d$agem) & is.na(d$agef) & is.na(d$weightm) & is.na(d$weightf))
d <- d[-no_parent_info, ]

expect_true(nrow(d) == 137)
# 137 values left, same as 2009 analysis


## Create Variables and Recreate 2009 Results

d$weightfm <- rowMeans(d[, c("weightm", "weightf")], na.rm = TRUE)
d$age2 <- d$age^2
d$age3 <- d$age^3
d$age4 <- d$age^4
d$parage <- rowMeans(d[, c("agem", "agef")], na.rm = TRUE)
d$parage2 <- d$parage^2
d$parage3 <- d$parage^3
d$parage4 <- d$parage^4
d$female <- as.numeric(d$sex == "0")
d$mom.only <- as.numeric(is.na(d[, "weightf"]))
d$dad.only <- as.numeric(is.na(d[, "weightm"]))
d$hhid <- paste("F", d$fid, "M",  d$mid, sep = "")

m0 <- lm(weight ~ weightfm + age + age2 + parage + parage2 + female + dad.only + mom.only, data = d)
expect_true(abs(logLik(m0) - (-418.71)) < 0.1)
display.er(m0, cluster = d$hhid)

# ** Publish Beta here:  0.509 (0.128) 

display.cr(m0, cluster=d$hhid)
