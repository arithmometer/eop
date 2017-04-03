#!/usr/bin/env Rscript
# create directories
dir.list <- c("ssa", "pul", "csv", "today", "ssa/params")
lapply(dir.list, function(path) {
  dir.create(path, showWarnings = FALSE)
})

# get forecasts from Pulkovo website
write.csv(55433, "pul/last.csv")
source("download_pul.R")

# generate forecast
source("generate_forecast.R")

# don't forget to configure your CRON!