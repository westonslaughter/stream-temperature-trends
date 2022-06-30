### ---------------------------
##
## Title: merge.R
##
## Purpose: this munges and merges data sources together
##
## Author: WS
##
## Date Created: 2022-05-01
##
## ---------------------------
library(dplyr)
library(feather)
source("./src/helpers.R")

## Data
# USGS stream temp and discharge
raw_stream_temp <- read.csv("./data/dv/raw_temp_dv_compiled.csv")
raw_stream_disch <- read.csv("./data/dv/raw_disch_dv_compiled.csv")
daymet_files <- list.files("./data/air/air_temp/daymet", full.names = TRUE)
daymet_data <- featherCompiler(daymet_files)

# clean USGS data
# temp
stream_temp <- raw_stream_temp %>%
#   remove provisional data
    filter(X_00010_00001_cd == "A") %>%
    mutate(var = "wtr_temp") %>%
#   select desired columns
    select(dateTime, site_no, agency_cd, var, X_00010_00001) %>%
#   rename columns
    rename(
      date = dateTime,
      site_code = site_no,
      data_source = agency_cd,
      val = X_00010_00001
    ) %>%
  mutate(
    site_code = stringr::str_pad(string = site_code, width = 8, side = 'left', pad = 0)
  )

# Q
stream_disch <- raw_stream_disch %>%
#   remove provisional data
    filter(X_00060_00003_cd == "A") %>%
    mutate(var = "disch") %>%
#   select desired columns
    select(dateTime, site_no, agency_cd, var, X_00060_00003) %>%
#   rename columns
    rename(
      date = dateTime,
      site_code = site_no,
      data_source = agency_cd,
      val = X_00060_00003
    ) %>%
    mutate(
      site_code = stringr::str_pad(string = site_code, width = 8, side = 'left', pad = 0)
    )

# combine
stream_temp <- stream_temp %>%
  mutate(date = as.Date(date))
stream_disch <- stream_disch %>%
  mutate(date = as.Date(date))
daymet_data <- daymet_data %>%
  mutate(date = as.Date(date))

stream_long <- rbind(stream_temp, stream_disch)
temp_long <- rbind(stream_long, daymet_data)

# calculate z-scores
zcalc <- function(x) {
  (x - mean(x))/sd(x)
}


# annual
annual.df <- temp_long %>%
  tidyr::pivot_wider(
           id_cols = c('date', 'site_code'),
           names_from = var,
           values_from = val,
           ) %>%
  # daymet starts in 1980, so, cut to 1980
  tidyr::drop_na() %>%
  filter(date > as.Date("1980-01-01")) %>%
  mutate(
    year = format(as.Date(date), "%Y"),
    mean_air_temp = ((tmax..deg.c. + tmin..deg.c.)/2),
    wtr_air_ratio = wtr_temp/mean_air_temp,
    ) %>%
  select(-date) %>%
  group_by(site_code, year) %>%
  summarise(
    across(wtr_temp:wtr_air_ratio, mean, .names = "mean_{.col}"),
    across(wtr_temp:wtr_air_ratio, min, .names = "min_{.col}"),
    across(wtr_temp:wtr_air_ratio, max, .names = "max_{.col}")
  )

annual.df.z <- annual.df %>%
  tidyr::drop_na() %>%
  mutate(
    across(
      mean_wtr_temp:max_wtr_air_ratio, zcalc, .names = "z_{.col}")
  )

write_feather(annual.df.z, "./data/summary/annual_temp_z.feather")
