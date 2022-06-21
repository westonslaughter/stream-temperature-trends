### ---------------------------
##
## Title: site_code_retrieval.R
##
## Purpose: this script will query USGS for sites meeting our data requirements
##
## Author: WS
##
## Date Created: 2022-04-11
##
## ---------------------------

library(dataRetrieval)
library(dplyr)

# loading in USGS temp and discharge codes
tempCode <- "00010"
dischCode <- "00060"

# looping thru every US state
for (state in state.abb) {
  tryCatch(
    expr = {
      print(paste("attempting pull, USGS temp data: ", state))

      # get all sites with temp data for state
      sites_raw <- whatNWISdata(stateCd = state,
                                parameterCd = c(tempCode, dischCode)
                                )

      sites <- sites_raw %>%
          mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
          # > 30 years of data
          filter(period > 30*365)

    },
    error = function(e) {
      print(paste("failed to load sites with temp data:", state))
    },
    finally = {
      fp <- paste0("data/sites/codes/", state, ".csv")
      write.csv(sites, file = fp)
    }
  )
}
