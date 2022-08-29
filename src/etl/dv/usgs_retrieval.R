library(dplyr)
library(feather)
library(dataRetrieval)
source('src/etl/etl_helpers.R')

# this script will retrieve from USGS:
#   -temp
parameterCd <- '00010'
#   - streams
siteType <- "ST"
#   - daily (dv) temperature data (mean, max, and min)
service <- 'dv'
#   - starting at minimum in 1980
startDate <- as.Date('1980-01-01')
#   - going at minimum to 2021
endDate <- as.Date('2020-12-31')
#   - with minimum if 0.8 average observations per day
min_obs_day <- 0.8

## # retrieve dataframe of sites (and site info) that meet minimum criteria
## usgsSiteRetrieval(service, parameterCd, startDate, endDate, min_obs_day)

## # retrieve data for sites
## usgsDataRetrieval(readpath = 'data/dv/sites/sites.feather',
##                   writepath = 'data/dv/raw/wtr/')

# AIR, WATER, DISCHARGE RETRIEVAL
sites <- usgsSiteRetrieval(startDate=as.Date('1990-01-01'))

# filter to sites which meet reqs for water temp AND Q
t_sites <- unique(sites[sites$parm_cd == '00010',]$site_no)
q_sites <- unique(sites[sites$parm_cd == '00060',]$site_no)

qt_sites <- t_sites[t_sites %in% q_sites]
qt_sites_x <- sites[sites$site_no %in% qt_sites,] %>%
  group_by(site_no, station_nm) %>%
  summarize(begin_date = min(begin_date),
            end_date = min(end_date))

write_feather(airwtrq_sites, 'data/dv/sites/awq/sites.feather')

usgsDataRetrieval(readpath = 'data/dv/sites/sites.feather',
                  writepath = 'data/dv/raw/wtr/')
