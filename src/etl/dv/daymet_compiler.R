source("src/etl/etl_helpers.R")
library(dplyr)
library(stringi)
library(tidyr)
library(lubridate)
library(feather)

# site info
sites <- read_feather("data/dv//sites/sites.feather")

meta <- sites %>%
  group_by(site_code) %>%
  summarise(
    min = max(begin_year),
    max = min(end_year)
  )

daymet_data <- featherCompiler("data/dv/raw/air/daymet")

# wide form, by var
daymet_data <- daymet_data %>%
  pivot_wider(id_cols = c("site_code", "date"),
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
    air.tmean = air.tmax - air.tmin
  )

# pull in uSGS
usgs_data <- featherCompiler('data/dv/raw/wtr')

usgs_data <- usgs_data %>%
  select(
    dataset,
    site_code,
    datetime,
    max = X_00010_00001,
    min = X_00010_00002,
    mean = X_00010_00003
  ) %>% mutate(
    date = date(datetime)
        )

# merge
airwtr <- left_join(usgs_data, daymet_data, by = c('site_code', 'date'))

# save wide df
write_feather(airwtr, "data/dv/munged/airwtr/airwtr.feather")
airwtr <- read_feather("data/dv/munged/airwtr/airwtr.feather")
