library(dplyr)
library(data.table)
library(feather)

source("src/etl/etl_helpers.R")
# sites
infofiles <- dir("data/munged/sites/info", full.names = TRUE)
info <- fileCompiler(infofiles)
## write_feather(info, "./data/munged/sites/info_compiled.feather")

sitefiles <- dir("data/munged/sites/codes", full.names = TRUE)
sites <- fileCompiler(sitefiles)
## write_feather(sites, "./data/munged/sites/sites_compiled.feather")
