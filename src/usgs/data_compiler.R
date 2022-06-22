### ---------------------------
##
## Title: compiler.R
##
## Purpose: this compiles retrieved data, compartmentalized by state, into unified datasets
##
## Author: WS
##
## Date Created: 2022-04-29
##
## ---------------------------

## TODO: implement parallel processing
## library(parallel)
## numCores <- detectCores()
## temp <- mclapply(tempnames, stateCompiler, mc.cores = numCores)

source("src/helpers.R")

tempfiles <- dir("./data/dv/temp", full.names = TRUE)
temp <- fileCompiler(tempfiles)
fwrite(temp, "./data/dv/temp_compiled.csv")

dischfiles <- dir("./data/dv/disch", full.names = TRUE)
disch <- stateCompiler(dischfiles)
fwrite(disch, "./data/dv/disch_compiled.csv")

infofiles <- dir("./data/sites/info", full.names = TRUE)
info <- fileCompiler(infofiles)
fwrite(info, "./data/sites/info_compiled.csv")

sitefiles <- dir("./data/sites/codes", full.names = TRUE)
sites <- fileCompiler(sitefiles)
fwrite(sites, "./data/sites/sites_compiled.csv")
