# combine daily air and water temperature records
library(dplyr)
library(feather)

# pull in compiled raw data
## air.daymet <- read_feather("data/munged/dv/air/focal_daymet_compiled.feather")
## wtr.usgs <- read_feather("data/munged/dv/wtr/focal_usgs_compiled.feather")

# pull in long term dv
air.daymet <- read_feather("data/munged/dv/air/dv_longterm_daymet_compiled.feather")
wtr.usgs <- read_feather("data/munged/dv/wtr/dv_longterm_usgs_compiled.feather")

# pare down and filter
air.daymet <- air.daymet %>%
  select(
    site_code,
    date,
    daylight = dayl..s.,
    ppt = prcp..mm.day.,
    radiation = srad..W.m.2.,
    snow = swe..kg.m.2.,
    air.tmax = tmax..deg.c.,
    air.tmin = tmin..deg.c.,
    vapor = vp..Pa.
  ) %>%
  mutate(
    air.tmean = air.tmax - air.tmin/2
  )

wtr.usgs <- wtr.usgs %>%
  filter(
    X_00010_00001_cd == "A",
    X_00010_00002_cd == "A",
    X_00010_00003_cd == "A"
  ) %>%
  select(
    site_code = site_no,
    date = dateTime,
    wtr.tmax =  X_00010_00001,
    wtr.tmin =  X_00010_00002,
    wtr.tmean = X_00010_00003
  ) %>%
  mutate(date = as.Date(date))

# combine
air.wtr <- merge(wtr.usgs,
                 air.daymet,
                 by = c('site_code', 'date'))

write_feather(air.wtr, "data/munged/dv/combined/dv_longterm_air_wtr.feather")
