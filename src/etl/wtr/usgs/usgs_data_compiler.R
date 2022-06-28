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

source("src/etl/etl_helpers.R")
library(dplyr)
library(feather)
library(data.table)

# daily
tempfiles <- dir("data/raw/dv/wtr", full.names = TRUE)
temp <- fileCompiler(tempfiles)
## write_feather(temp, "data/munged/dv/wtr/usgs_compiled.feather")

dischfiles <- dir("./data/dv/disch", full.names = TRUE)
disch <- stateCompiler(dischfiles)
write_feather(disch, "./data/dv/disch_compiled.feather")

# grab
tempfiles <- dir("data/qw/temp", full.names = TRUE)
temp <- fileCompiler(tempfiles)
## write_feather(temp, "data/compiled/temp_qw.feather")

dischfiles <- dir("data/qw/disch", full.names = TRUE)
disch <- stateCompiler(dischfiles)
write_feather(disch, "./data/dv/disch_compiled.feather")

