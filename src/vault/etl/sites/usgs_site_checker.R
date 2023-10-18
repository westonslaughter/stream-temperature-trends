# exlporing site list from retrieval
library(dataRetrieval)
library(dplyr)

AL <- read.csv('data/munged/filter/sites/codes/AL.csv', colClasses = 'character')

AL.temp <- AL %>%
  filter(data_type_cd == 'dv',
         parm_cd == '00010')


AL.q <- AL %>%
  filter(data_type_cd == 'dv',
         parm_cd == '00060')

AL.q$site_no %in% AL.q$site_no


sites_raw <- whatNWISdata(stateCd = "AL",
                          parameterCd = c(tempCode, dischCode)
                          )
sites <- sites_raw %>%
    # streams only
    filter(site_tp_cd == "ST") %>%
  mutate(
    period = as.Date(end_date) - as.Date(begin_date),
    mean_obs_day = count_nu/as.numeric(period)
  ) %>%
  filter(
    period > period_min,
    mean_obs_day > obs_day_min
    )


 ## 15008000


sites_raw <- whatNWISdata(
                          siteNumber = '15008000',
  parameterCd = c('00010', '00060'),
  service = 'dv'
  )
