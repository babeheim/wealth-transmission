
rm(list = ls())

source("./project_support.r")

tic.clearlog()

tic("fit all models")
datasets_available <- list.files("./data", full.names = TRUE)
analyses_to_run <- datasets_available
analyses_to_run <- gsub("\\./data", "./site_scripts", analyses_to_run)
analyses_to_run <- gsub("\\.csv", ".r", analyses_to_run)
x <- sapply(analyses_to_run, source)
toc(log = TRUE)
