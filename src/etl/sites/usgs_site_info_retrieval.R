### ---------------------------
##
## Title: site_info_retrieval.R
##
## Purpose: this script will query information about USGS sites
##
## Author: WS
##
## Date Created: 2022-06-21
##
## ---------------------------

library(dataRetrieval)
library(dplyr)
library(data.table)

source("src/etl/etl_helpers.R")


## site info retrieval
stateInfoRetrievalLoop(
  "data/sites/codes/",
  "data/sites/info/"
)
