#!/usr/bin/env Rscript
library(methods)

args = commandArgs(trailingOnly=TRUE)
start.forecast <- as.integer(args[1])

source("forecast.R")
source("generate.R")
