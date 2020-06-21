
rm(list = ls())

source("./project_support.r")

## Clean Data Following 2009 Paper

data <- read.csv("./data/tsimane_labor_ties.csv", stringsAsFactors = FALSE)
expect_true(nrow(data) == 67)

ties <- data[,"sons_numhelpersnopay"]
tiesf <- data[,"dads_numhelpersnopay"]
age <- data[,"sons_edad"]
age2 <- age^2
agef <- data[,"dads_edad"]
agef2 <- agef^2
fid <- data[,"dads_midpid"]

m0 <- lm(ties ~ tiesf + age + age2 + agef + agef2)
# *** Published Beta: 0.181 (0.106)

display.er(m0, cluster=fid)
display(m0, digits=3)
robust.se(m0, fid)
display.cr(m0, cluster=fid)
sdd(tiesf)/sdd(ties)
mean(tiesf)/mean(ties)


