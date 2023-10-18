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

# set our filtering values
period_min <- 20*365 # 40 years
## year_min <- 1980
obs_day_min <- .75 # ~75% of days sampled

# looping thru every US state
state = "MD"
for (state in state.abb) {
  tryCatch(
    expr = {
      print(paste("attempting pull, USGS site metadata: ", state))

      # get all sites with temp data for state
      sites_raw <- whatNWISdata(stateCd = state,
                                service = 'dv',
                                parameterCd = c(tempCode)
                                )

      sites <- sites_raw %>%
        filter(site_tp_cd == "ST") %>%
        mutate(
          period = as.Date(end_date) - as.Date(begin_date),
          mean_obs_day = count_nu/as.numeric(period)
        )

      sites_t <- sites %>%
        filter(
          period > period_min,
          mean_obs_day > obs_day_min,
          parm_cd == tempCode
          )

    },
    error = function(e) {
      print(paste("failed to load sites with temp data:", state))
    },
    finally = {
      fp <- paste0("data/munged/filter/sites/codes/", state, ".csv")
      write.csv(sites, file = fp)
      print(paste("----DONE:", state, "number of sites:", n))
    }
  )
}
