library(dataRetrieval)
library(dplyr)
library(mda.streams)

tempCode <- "00010"
dischCode <- "00061"

# state by state for loop, to identify temp sites in USGS
for (state in state.abb) {
  tryCatch(
    expr = {
      print(paste("attempting pull, USGS temp data: ", state))

      # get all sites with temp data for state
      sitesTemp <- whatNWISdata(stateCd = state,
                            parameterCd = c(tempCode, dischCode))

      # > 10 years of data
      sitesTemp_filter <- sitesTemp %>%
          mutate(period = as.Date(end_date) - as.Date(begin_date)) %>%
          filter(period > 10*365)

      # keep only continuous data
      sitesTemp_dv <- sitesTemp_filter %>%
        filter(data_type_cd == "dv")

    },
    error = function(e) {
      print(paste("failed to load sites with temp data:", state))
    },
    finally = {
      fp <- paste0("data/vars/dv/", state, ".csv")
      write.csv(sitesTemp_dv, file = fp)
    }
  )
}
