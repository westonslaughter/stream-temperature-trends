library(dplyr)
library(data.table)
library(feather)
# chesapeake data?
cbm <- read.csv("data/raw/chesapeake/WaterQualityWaterQualityStation.csv") %>%
  select(
    site_code = MonitoringStation,
    Agency,
    date = SampleDate,
    val = MeasureValue,
    lat = Latitude,
    long = Longitude
  ) %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    year = lubridate::year(date)
  )

cbm_stats <- cbm %>%
  group_by(site_code, lat, long) %>%
  summarize(
    n = length(date),
    period_yrs = max(year) - min(year)
    ) %>%
  arrange(desc(period_yrs), desc(n))
cbm_sites <- cbm_stats %>%
  filter(period_yrs > 15, n > 300) %>%
  pull(site_code)

cbm_munged <- cbm %>%
  filter(site_code %in% cbm_sites)
write_feather(cbm_munged, "data/dv/raw/cbm/cbm.feather")
write_feather(cbm_stats, "data/dv/sites/cmb_sites.feather")


# California?
## ssi <- read.csv('./data/dv/raw/ca_nevada_county/deer_creek.csv')
