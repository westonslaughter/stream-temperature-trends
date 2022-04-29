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

ls <- state.abb[1:13]
years <- c(2019, 2020, 2021, 2022)

results_n <- length(ls) * length(years)

results = matrix('',
                  ncol = 2,
                  nrow = results_n
                )
​

step <- 1:length(ls)

for(yr in years) {
  for(x in step){
    results[x, 1] <- ls[which(step == x)]
    results[x, 2] <- yr
  }
  step <- step + length(ls)
}


## x <- read.csv("./data/sites/codes/AK.csv")
## test <- unique(x$site_no)

## begin <- min(x$begin_date)
## end <- max(x$end_date)

## qw_codes <- paste0("USGS-", test)
##             # get grab sample data
## info <- readWQPqw(
##               siteNumbers = qw_codes,
##               parameterCd = "00010",
##               startDate = begin,
##               endDate = end
##             )
## info <- readNWISdata(
##             sites = test,
##             parameterCd = "00060",
##             service = "dv",
##             startDate = begin,
##             endDate = end
##           )
