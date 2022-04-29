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

source("./R/helpers.R")

# pull all 'dv' temp data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/dv/temp/"
)

# pull all 'dv' disch data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/dv/disch/",
  varid = "00060",
)

# pull all 'qw' temp data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/qw/temp/",
  svc = "qw"
)

# pull all 'qw' disch data
stateRetrievalLoop(
  "./data/sites/codes/",
  "./data/qw/disch/",
  varid = "00060",
  svc = "qw"
)

