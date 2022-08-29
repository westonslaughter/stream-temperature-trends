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


# retrieve dataframe of sites (and site info) that meet minimum criteria
usgsSiteRetrieval(service, parameterCd, startDate, endDate, min_obs_day)

# load in sites
sites <- read_feather('data/dv/sites/sites.feather')

usgsDataRetrieval <- function(readpath,
                              writepath,
                              parameterCd = "00010",
                              service = "dv",
                              siteType="ST") {
  print(paste("USGS data retrieval:", parameterCd))

    tryCatch(
      expr = {
        sites <- read_feather(readpath)

        if(nrow(sites) == 0) {
          print(paste("---- WARNING:", state, "input CSV is empty"))
        } else {
          codes <- unique(sites$site_code)

          # get daily mean data
          for(site in codes) {
            info <- readNWISdata(
              sites = site,
              parameterCd = parameterCd,
              service = service,
              startDate = startDate
            ) %>% rename(
                    dataset = agency_cd,
                    site_code = site_no,
                    datetime = dateTime
                  )

            print(paste('--- saving', site, 'to feather at', writepath))

            fp <- paste0(writepath, site, '.feather')
            write_feather(info, fp)
          }
        }
        print(paste0("---- ", service, ": DONE"))
      },
      error = function(e) {
        print(paste("---- ERROR:", site))
      }
    )
  }


# retrieve data for sites
usgsDataRetrieval(readpath = 'data/dv/sites/sites.feather',
                  writepath = 'data/dv/raw/wtr/')

# DISCHARGE RETRIEVAL
airwtrq_sites <- usgsSiteRetrieval()
write_feather(airwtrq_sites, 'data/dv/sites/awq/sites.feather')

## # this script will retrieve from USGS:
## #   -temp and discharhge
## parameterCd <- c('00010', '00060')
## #   - streams
## siteType <- "ST"
## #   - daily (dv) temperature data (mean, max, and min)
## service <- 'dv'
## #   - starting at minimum in 1980
## startDate <- as.Date('1980-01-01')
## #   - going at minimum to 2021
## endDate <- as.Date('2020-12-31')
## #   - with minimum if 0.8 average observations per day
## min_obs_day <- 0.8

## # main stats
## statCd <- c('00001', '00002', '00003')

## testNWIS <- whatNWISdata(
##             stateCd = 'MD',
##             parameterCd = parameterCd,
##             service = 'dv',
##             startDt = startDate,
##             endDt = endDate,
##             siteType = 'ST',
##             statCd = '00003'
## ) %>%
##   filter(lubridate::year(begin_date) < 1981,
##          lubridate::year(end_date) > 2020) %>%
##   mutate(diff_dates = end_date - begin_date) %>%
##   filter(count_nu > (diff_dates * 0.9))

## ## metaNWIS <- testNWIS %>%
## ##   group_by(site_code) %>%
## ##   mutate(
## ##     n_temp
## ##   )
