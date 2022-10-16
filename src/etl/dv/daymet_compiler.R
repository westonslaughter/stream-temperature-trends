source("src/etl/etl_helpers.R")
library(dplyr)
library(stringi)
library(tidyr)
library(lubridate)
library(feather)

# site info
## sites <- read_feather("data/dv/sites/awq/sites.feather")

site_picks <- unique(as.character(sites$site_no))
for(i in 1:length(site_picks)) {
  code <- site_picks[i]

  if(substr(code,0,1) != 0) {
    site_picks[i] <- paste0('0', code)
  }
}

## meta <- sites %>%
##   group_by(site_no) %>%
##   summarise(
##     min = max(begin_date),
##     max = min(end_date)
##   )


daymet_data <- featherCompiler("data/dv/raw/air/daymet",)

# wide form, by var
daymet_data <- daymet_data %>%
  pivot_wider(id_cols = c("site_no", "date"),
              names_from = var,
              values_from = val) %>%
  rename(
    daylength = dayl..s.,
    ppt = prcp..mm.day.,
    radiation = srad..W.m.2.,
    snow = swe..kg.m.2.,
    air.tmax = tmax..deg.c.,
    air.tmin = tmin..deg.c.,
    vapor = vp..Pa.
  ) %>%
  mutate(
    air.tmean = (air.tmax + air.tmin)/2
  )

# pull in uSGS
usgs_data <- featherCompiler('data/dv/raw/awq', site_picks)

## for(file in list.files('data/dv/raw/awq', full.names = TRUE)) {
##   print(file)
##   ## if(substr(file,0,1) != 0) {
##   ##   file.rename(file, paste0('0', code))
##   ## }
## }

usgs_data <- usgs_data %>%
  select(
    dataset,
    site_no = site_code,
    datetime,
    max = X_00010_00001,
    min = X_00010_00002,
    mean = X_00010_00003,
    q.max = X_00060_00001,
    q.min = X_00060_00002,
    q.mean = X_00060_00003
  ) %>% mutate(
    date = date(datetime)
        )

# merge
airwtrq <- left_join(usgs_data, daymet_data, by = c('site_no', 'date'))

# save wide df
write_feather(airwtrq, "data/dv/munged/airwtr/airwtrQ.feather")
airwtrQ <- read_feather("data/dv/munged/airwtr/airwtrQ.feather")
