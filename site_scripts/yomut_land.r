
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/yomut_land.csv", stringsAsFactors = FALSE)

expect_true(nrow(data) == 58)
# 58 values left, same as 2009 analysis

## Create Variables and Recreate 2009 Results

patrimonyf <- data[,"patrimonyf"]
patrimony <- data[,"patrimony"]
hid <- data[,"hid"]

m0 <- lm(patrimony ~ patrimonyf)
# *** Published Beta: 0.010 (0.028)

display.er(m0)
display(m0, digits=3)
display.cr(m0)
display.er(m0, cluster=hid)
