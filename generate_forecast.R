#!/usr/bin/env Rscript
library(methods)

date.string <- format(Sys.time(), "%Y-%m-%d")

# put to start.forecast MJD of today
start.forecast <- as.integer(as.Date(date.string) - as.Date("1858-11-17"))

source("forecast.R")

finals2000 <- read.csv(paste("csv/", start.forecast - 1, "/finals2000A.daily.csv", sep=""), sep=";")

last.mjd.c04 <- c04[nrow(c04), "MJD"]
mjd.from <- last.mjd.c04 + 1
mjd.to <- start.forecast - 1
ind.from <- which(finals2000["MJD"] == mjd.from)
ind.to <- which(finals2000["MJD"] == mjd.to)

gap.df <- data.frame("MJD"=mjd.from:mjd.to,
                     "x"=finals2000[ind.from:ind.to, "x_pole"],
                     "y"=finals2000[ind.from:ind.to, "y_pole"],
                     "LOD"=finals2000[ind.from:ind.to, "LOD"] / 1000,
                     "dX"=finals2000[ind.from:ind.to, "dX"] / 1000,
                     "dY"=finals2000[ind.from:ind.to, "dY"] / 1000)
gap.df[nrow(gap.df), "LOD"] <- gap.df[nrow(gap.df) - 1, "LOD"]

c04 <- rbind(c04[, c("MJD", "x", "y", "LOD", "dX", "dY")], gap.df)

source("generate.R")
source("generate_today.R")
