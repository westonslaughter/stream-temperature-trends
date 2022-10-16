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
library(stringr)
read_plus <- function(flnm) {
    read_csv(flnm) %>%
        mutate(site = paste0("ssi_", stringr::str_match(flnm, "[0-9]+")))
}

ssi.sites <- read_plus('data/raw/ca_nevada_county/ssi/Map_data.csv') %>%
  mutate(site_code = paste0("ssi_", stringr::str_match(Site1, "[0-9]+")),
         lat = Lat,
         long = Long
         )

ssi <- list.files('data/raw/ca_nevada_county/ssi', full.names = TRUE) %>%
  map_df(~read_plus(.))


write_feather(ssi, "data/dv/raw/ssi/ssi.feather")
write_feather(ssi.sites, "data/dv/sites/ssi_sites.feather")
