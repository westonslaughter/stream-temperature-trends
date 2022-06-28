### ---------------------------
##
## Title: retrieval.R
##
## Purpose: this uses the site codes created by the site scripts to retrieve temperature and discharge data for each site
##
## Author: WS
##
## Date Created: 2022-04-11
##
## ---------------------------
library(dataRetrieval)
library(data.table)
library(dplyr)

source("src/etl/etl_helpers.R")
source('src/etl/wtr/usgs/usgs_helpers.R')

## data retrieval
# 'dv' temp data
stateRetrievalLoop(
  readpath = "./data/munged/sites/codes/",
  writepath = "./data/raw/dv/wtr/",
  site_filter = unique(sites$site_code)
)

# 'dv' disch data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/dv/disch/",
  varid = "00060",
)

# 'qw' temp data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/qw/temp/",
  svc = "qw"
)

# 'qw' disch data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/qw/disch/",
  varid = "00060",
  svc = "qw"
)
