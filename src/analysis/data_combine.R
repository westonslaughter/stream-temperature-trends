# script going thru our intro questions sequentially and addressing with stats and plots
library(dplyr)
library(data.table)
library(feather)
library(lubridate)

## prep
# combine USGS stream temp with daymet data
# DAYMET
daymet.raw <- read_feather("data/compiled/daymet.feather")

daymet <- daymet.raw %>%
  rename(
   day_length = dayl..s.,
   ppt = prcp..mm.day.,
   radiation = srad..W.m.2.,
   snow = swe..kg.m.2.,
   air_max = tmax..deg.c.,
   air_min = tmin..deg.c.,
   vapor_pressure = vp..Pa.,
  )

# USGS
usgs.raw <- fread("data/compiled/temp_qw.csv", colClasses = "character")

# continuous
usgs <- usgs.raw %>%
  separate(MonitoringLocationIdentifier, c("data_source", "site_code")) %>%
  select(site_code,
         datetime = ActivityStartDateTime,
         temp = ResultMeasureValue,
         status = ResultStatusIdentifier
         ) %>%
  mutate(
    date = date(as.Date(datetime))
  )

# daily vals (max, min, mean)
usgs.dv <- usgs %>%
  filter(status == "Accepted") %>%
  mutate(
    temp = as.numeric(temp)
  ) %>%
  group_by(site_code, date) %>%
  summarise(
    wtr_max = max(temp),
    wtr_min = min(temp),
    wtr_mean = mean(temp, na.rm = TRUE)
  )

# merged df
air_wtr <- merge(usgs.dv, daymet, by = c('site_code', 'date'))
fwrite(air_wtr, "data/compiled/air_wtr_compiled.csv")

## analysis
# are stream temperatures changing?
