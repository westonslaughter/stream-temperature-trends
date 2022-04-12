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

## x <- read.csv("./data/sites/codes/AK.csv")
## test <- unique(x$site_no)

## begin <- min(x$begin_date)
## end <- max(x$end_date)

## info <- readNWISdata(
##             sites = test,
##             parameterCd = "00060",
##             service = "dv",
##             startDate = begin,
##             endDate = end
##           )
